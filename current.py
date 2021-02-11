#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import geojson
import requests
from shapely.geometry import box, Polygon, MultiPolygon, GeometryCollection, shape
from shapely import wkt
from shapely.ops import transform
from functools import partial
import pyproj
import time
import json
from retry import retry
import pandas as pd
import csv
import warnings


#These functions are taken from the module osm-road-length
#They have been adapted to count cycle lanes as well

def _simplify(s, threshold, delta=0.005):

    while not _check_length(s, threshold):
        s = s.simplify(delta, False)
        delta = delta + 0.005

    return s


def _check_length(s, threshold):

    return len(str(s)) < threshold


def get_area(s):
    s = shape(s)
    proj = partial(
        pyproj.transform, pyproj.Proj(init="epsg:4326"), pyproj.Proj(init="epsg:3857")
    )

    return transform(proj, s).area / 1e6  # km


def threshold_func(g, value):

    return get_area(g) < value


def katana(geometry, threshold_func, threshold_value, count=0):
    """Split a Polygon into two parts across it's shortest dimension"""
    bounds = geometry.bounds
    width = bounds[2] - bounds[0]
    height = bounds[3] - bounds[1]
    if threshold_func(geometry, threshold_value) or count == 250:
        # either the polygon is smaller than the threshold, or the maximum
        # number of recursions has been reached
        return [geometry]
    if height >= width:
        # split left to right
        a = box(bounds[0], bounds[1], bounds[2], bounds[1] + height / 2)
        b = box(bounds[0], bounds[1] + height / 2, bounds[2], bounds[3])
    else:
        # split top to bottom
        a = box(bounds[0], bounds[1], bounds[0] + width / 2, bounds[3])
        b = box(bounds[0] + width / 2, bounds[1], bounds[2], bounds[3])
    result = []
    for d in (
        a,
        b,
    ):
        c = geometry.intersection(d)
        if not isinstance(c, GeometryCollection):
            c = [c]
        for e in c:
            if isinstance(e, (Polygon, MultiPolygon)):
                result.extend(katana(e, threshold_func, threshold_value, count + 1))
    if count > 0:
        return result
    # convert multipart into singlepart
    final_result = []
    for g in result:
        if isinstance(g, MultiPolygon):
            final_result.extend(g)
        else:
            final_result.append(g)
    return final_result


def to_geojson(x):

    if isinstance(x, MultiPolygon):
        x = max(x, key=lambda a: a.area)

    g = geojson.Feature(geometry=x, properties={}).geometry

    return g["coordinates"][0]


def swipe(x):
    return [[c[1], c[0]] for c in x]


def flatten(l):
    return [str(round(item, 4)) for sublist in l for item in sublist]


def to_overpass_coords(x):

    coords = to_geojson(x)
    coords = swipe(coords)
    coords = flatten(coords)
    coords = " ".join(coords)
    return coords

@retry(tries=60)
def overpass_request(geo,ways):

    overpass_url = "http://overpass-api.de/api/interpreter"
    
    overpass_query = (
         """[timeout:1200][out:json];
""")
    
    for way in ways:
        overpass_query += (way + """(poly:"%s");
make stat length=sum(length());
out;
        
"""
            % geo
        )
    
    response = requests.get(overpass_url, params={"data": overpass_query},headers = {'User-agent': 'cycleLanes'})
    if response.status_code == 429:
        time.sleep(60)
    
    print(response)
    
    return [e["tags"]["length"] for e in response.json()["elements"]]


def get(geometry, way, threshold_value=1000000, simplify=True, simplify_limit=1000):
    """Get Open Street Maps highways length in meters and object count for a given geometry

    It splits the regions to manage overpass turbo limits.

    For MultiPolygons, only the biggest polygon will be considered.

    Parameters
    ----------
    geometry : shapely.geometry
        A shapely polygon
    threshold_value : int, optional
        Maximum area in sq km to split the polygons, by default 1000000
    simplify: bool, optional
        Simplify polygon to maximum number of characters
    simplify_limit: int, optional
        Maximum number of characters of WKT shape

    Returns
    -------
    pd.DataFrame
        Table indexed by highway with length sum in meters and observation count
    """

    geometries = katana(geometry, threshold_func, threshold_value)

    for geo in geometries:

        if simplify:
            geo = _simplify(geo, simplify_limit)

        geo = to_overpass_coords(geo)
        responses = overpass_request(geo, way)
        

    return responses

print('Begin')
warnings.filterwarnings("ignore", category=FutureWarning)


#The OSM tags for the seven types of cycling infrastructure considered
#Constructed in such a way to prevent double counting
#Allowing opposite direction cycling is not considered infrastructure for this purpose
ways = ['way[highway]', #All roads and paths
        'way[highway=cycleway]', #Cycle ways
        'way[highway][highway!=cycleway][motor_vehicle=no]', #Roads without motor traffic
        'way[highway][highway!=cycleway][motor_vehicle!=no][vehicle=no]', #Roads without motor traffic again
        'way[highway!=cycleway][motor_vehicle!=no][vehicle!=no][bicycle=designated]', #Designated bicyle areas
        'way[highway!=cycleway][motor_vehicle!=no][vehicle!=no][bicycle!=designated][cycleway][cycleway!=opposite][cycleway!=separate][cycleway!=no]', #Cycle lanes
        'way[highway!=cycleway][motor_vehicle!=no][vehicle!=no][bicycle!=designated]["cycleway:left"]', #Left Cycle lanes
        'way[highway!=cycleway][motor_vehicle!=no][vehicle!=no][bicycle!=designated]["cycleway:right"]' #Right Cycle lanes
        ]

#Cities considered. Netherlands included to add nationwide average
cities = ['London','Cambridge','Leicester','Liverpool','Cardiff','Birmingham',
          'ParisMetropolitan','Berlin','Copenhagen','Amsterdam','Rotterdam','The Hague',
          'Utrecht','Eindhoven','Groningen','Vienna','Brussels','Madrid','Rome',
          'Stockholm','Helsinki','Warsaw','Newcastle','Manchester','Glasgow','Bristol',
          'Oxford','Edinburgh','Leeds','Dublin','Zurich','Netherlands']

currentData = []

#Loops through every city, saves output to csv
#Should only take two hours or so, depending on Overpass rate limiting
with open('current2.csv',mode='w') as f:
    
    writer = csv.writer(f, delimiter=',')
    
    for city in cities:
             
        filename = './shapefiles/' + city + '.txt'
        with open(filename, 'r') as f:
            shapefile = f.read()
        
        geometry = wkt.loads(shapefile)
    
        d = get(geometry,ways)
        road = float(d[0])
        bike = sum([float(i) for i in d[1:]])
        print(city, road, bike, bike/road)
        print(d)
        currentData.append([city, road, bike, bike/road] + d)
        
        #write to CSV file
        writer.writerow([city, road, bike, bike/road] + d)
        
        #sleep for a while to get around overpass rate limiting
        time.sleep(1)
            
print(currentData)