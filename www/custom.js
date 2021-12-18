// create js function that triggers a click on a marker selected by a row in a DT
// specificly for the geographicExplore map and linked to onRender defined as 'map'
// https://stackoverflow.com/questions/70288989/programatically-trigger-marker-mouse-click-event-in-r-leaflet-for-shiny/
shinyjs.markerClick = function(id) {
                        // remove any tooltips that are present to prevent them cluttering the map
                        map.eachLayer(function(layer) {
                          if(layer.options.pane === "tooltipPane") layer.removeFrom(map);
                        });
                        // activate a click on the relevant map marker
                        map.eachLayer(function (layer) {
                          if (layer.options.layerId == id) {
                            layer.fire("click");
                            }
                          })};