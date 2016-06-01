// Directions! Integrate w/ 3rd party turn-by-turn directions group. Using mapzen - which uses OSM. Check out their docs (in exercise 9).
// Start w/ blank feature layer again.
var routeLine = L.mapbox.featureLayer().addTo(map);
// Holds moused-over section of the route to highlight it.
var routeHighlight = L.mapbox.featureLayer().addTo(map); 

// Need function to take from point and to point and call their service
function getDirections(frm,to) {
  var jsonPayload = JSON.stringify({
    locations: [
      {lat: frm[1], lon: frm[0]},
      {lat: to[1], lon: to[0]}
    ],
        // tell it how to prioritize routes
        costing: 'pedestrian',
        directions_options: {
          units: 'miles'
        }
    })

  // now need a way to call the service. Since we are using jquery (access it using $).
  $.ajax({
    url:'https://valhalla.mapzen.com/route',
    data:{
      json: jsonPayload,
      api_key: 'valhalla-gwtf3x2'
    }
  }).done(function(data){
    var routeShape = polyline.decode(data.trip.legs[0].shape);
    routeLine.setGeoJSON({
      type:'Feature',
      geometry:{
        type:'LineString',
        coordinates: routeShape
      },
      properties:{
        "stroke": '#ed23f1',
        "stroke-opacity": 0.8,
        "stroke-width": 8
      }
    })

    $('#directions').fadeIn(400, function(){
      $('#summary').empty();
      $('#distance').text((Math.round(data.trip.summary.length * 100) / 100) + data.trip.units);
      $('#time').text((Math.round(data.trip.summary.time / 60 * 100) / 100) + ' min');

      data.trip.legs[0].maneuvers.forEach(function(item){
      var direction = '';
      direction += '<li class="instruction" data-begin=' + item.begin_shape_index + ' data-end=' + item.end_shape_index + '>';
      if(item.verbal_post_transition_instruction) direction += '<p class="post-transition">' + item.verbal_post_transition_instruction + '</p>';
      if(item.verbal_pre_transition_instruction) direction += '<p class="pre-transition">' + item.verbal_pre_transition_instruction + '</p>';
      direction += '</li>';
      $('#summary').append(direction);

    })

  })

      $('.instruction').on('mouseover', function(){
      var begin = Number($(this).attr('data-begin'));
      var end = Number($(this).attr('data-end'));

      routeHighlight.setGeoJSON({
        type:'Feature',
        geometry:{
          type: begin === end ? 'Point' : 'LineString',
          coordinates: begin === end ? routeShape.slice(begin)[0] : routeShape.slice(begin,(end + 1))
        },
        properties:{
          "stroke": '#1ea6f2',
          "stroke-opacity": 0.9,
          "stroke-width": 10,
          "marker-color": '#1ea6f2',
          "marker-size": 'small',
          "marker-symbol": 'star'
        }
      })
    }) // end mouseover instruction

    // handles what to do when user mouses out of the sidebar - remove highlight from the map.
    $('.instruction').on('mouseout', function(){
      routeHighlight.clearLayers()
    })


}) // end .done


} // End getDirections

// all the way to the end of our file
map.on('click', function(){
  routeLine.clearLayers();
})
 

        
   