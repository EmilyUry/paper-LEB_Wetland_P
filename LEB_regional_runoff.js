
// Lake Erie basin boundary and subwatershed boundaries
var LEB_boundary = ee.FeatureCollection("users/uryemily/LEB_boundary"),
    LEB_watersheds = ee.FeatureCollection("users/uryemily/LEB_watersheds"),
    LEB_HUC4 = ee.FeatureCollection("users/uryemily/LEB_HUC4_boundaries");

// Filter to each HUC4 subbasin
var ClairDetroit = ee.FeatureCollection("users/uryemily/LEB_HUC4_boundaries")
.filter("name == 'St. Clair-Detroit'").first().geometry();
var WesternLE = ee.FeatureCollection("users/uryemily/LEB_HUC4_boundaries")
.filter("name == 'Western Lake Erie'").first().geometry();
var SouthernLE = ee.FeatureCollection("users/uryemily/LEB_HUC4_boundaries")
.filter("name == 'Southern Lake Erie'").first().geometry();
var EasternLE = ee.FeatureCollection("users/uryemily/LEB_HUC4_boundaries")
.filter("name == 'Lake Erie'").first().geometry();
var NorthernLE = ee.FeatureCollection("users/uryemily/LEB_HUC4_boundaries")
.filter("name == 'Northern Lake Erie'").first().geometry();

// Add these boundary layers to the map
Map.setCenter(-80.547, 41.91, 7);
Map.addLayer(LEB_watersheds, {color: 'FF0000'}, 'sub basins');
//Map.addLayer(LEB_boundary, {color: '000000'}, 'LEB boundary');
//Map.addLayer(LEB_HUC4, {color: '000000'}, 'HUC4');
Map.addLayer(ClairDetroit, {color: '0000FF'}, 'ClairDetroit');
Map.addLayer(WesternLE, {color: '00FF00'}, 'WesternLE');
Map.addLayer(SouthernLE, {color: '0000FF'}, 'SouthernLE');
Map.addLayer(EasternLE, {color: '00FF00'}, 'EasternLE');
Map.addLayer(NorthernLE, {color: '00FF00'}, 'NorthernLE');


// Define range for a 30-year time series
var starDate = '1992-01-01';
var endDate = '2021-12-31';


// Call in global runoff data from TerraClimate for our time period
var Monthly = ee.ImageCollection("IDAHO_EPSCOR/TERRACLIMATE").select('ro')
  .filterDate(starDate, endDate);
  
// Get a list of all months in the time frame of interest
var allDates = ee.List(Monthly.aggregate_array('system:time_start'));
// Rename dates to simpler format
var allDatesSimple = allDates.map(function(date){
  return ee.Date(date).format().slice(0,10);
  });

// Function for finding mean runoff for each region of interest
var getRunoff = function(image) {
  var value_ro = ee.Image(image)
    .reduceRegion(ee.Reducer.mean(), NorthernLE) // Change this line for desired region
    .get('ro');
  return ee.Number(value_ro);
};

// Combine runoff result with list of dates
var count = Monthly.size();
var ro_list = Monthly.toList(count).map(getRunoff);
var paired = allDatesSimple.zip(ro_list);
// print ("paired", paired);  // print to check

// Turn paired list into a feature collection
var myFeatures = ee.FeatureCollection(paired.map(function(el){
  el = ee.List(el); // cast every element of the list
  return ee.Feature(null, {
    'date': ee.String(el.get(0)),
    'ro':ee.Number(el.get(1))
  });
}));

// Export feature collection, specifying corresponding names.
Export.table.toDrive(myFeatures,
"export_ro", //my task
"LEB climate data", //my export folder
"ro_region",  //file name
"CSV");
