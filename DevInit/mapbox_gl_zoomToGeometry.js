function zoomToGeometry(geometry,zoomPadding = 20){
    if(geometry.type=="Polygon"){
        var coordinates = geometry.coordinates[0];
        var bounds = coordinates.reduce(function(bounds, coord) {
            return bounds.extend(coord);
        }, new mapboxgl.LngLatBounds(coordinates[0], coordinates[0]));
    }
    if(geometry.type=="MultiPolygon"){
        var coordinates = geometry.coordinates[0][0];
            var bounds = coordinates.reduce(function(bounds, coord) {
                return bounds.extend(coord);
            }, new mapboxgl.LngLatBounds(coordinates[0], coordinates[0]));   
        for(var i = 1; i < geometry.coordinates.length; i++){
            var coordinates = geometry.coordinates[i][0];
            bounds.extend(coordinates.reduce(function(bounds, coord) {
                return bounds.extend(coord);
            }, new mapboxgl.LngLatBounds(coordinates[0], coordinates[0])));   
        }
    }
    map.fitBounds(bounds, {
        padding: zoomPadding
    });
}