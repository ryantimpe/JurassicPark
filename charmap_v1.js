// !preview r2d3 data = read.csv("CharacterPaths.csv")
//
// r2d3: https://rstudio.github.io/r2d3
//

var xScale = d3.scaleLinear().domain([-10, 10]).range([0, width]);
var yScale = d3.scaleLinear().domain([-10, 10]).range([height, 0]);

// Plot of locations on map

var datMap = [{"Location":"Big tree in clearing","Loc_map":"Isla Nublar","x":4,"y":0},{"Location":"Brachiosaurus hill","Loc_map":"Isla Nublar","x":-2,"y":-3},{"Location":"Dilophosaurus paddock","Loc_map":"Isla Nublar","x":1,"y":4},{"Location":"Fields","Loc_map":"Isla Nublar","x":2,"y":1},{"Location":"Gates","Loc_map":"Isla Nublar","x":-1,"y":-1},{"Location":"Landing pad","Loc_map":"Isla Nublar","x":-1,"y":-6},{"Location":"Maintenance shed","Loc_map":"Isla Nublar","x":-6,"y":-1},{"Location":"Park fence","Loc_map":"Isla Nublar","x":-2,"y":-2},{"Location":"Road to docks","Loc_map":"Isla Nublar","x":0.75,"y":3.75},{"Location":"The cars","Loc_map":"Isla Nublar","x":4,"y":3},{"Location":"Triceratops exhibit","Loc_map":"Isla Nublar","x":4,"y":5},{"Location":"Tyrannosaur paddock","Loc_map":"Isla Nublar","x":4,"y":3},{"Location":"Tyrannosaur paddock | Restroom","Loc_map":"Isla Nublar","x":4,"y":3},{"Location":"Velociraptor paddock","Loc_map":"Isla Nublar","x":-5,"y":0},{"Location":"Visitor's Center","Loc_map":"Isla Nublar","x":-4,"y":2}];

d3.select("svg").selectAll("circle.loc")
  .data(datMap)
  .enter()
  .append("circle")
  .attr("class", "locs")
  .attr("r", 3)
  .attr("cx", d => xScale(d.x))
  .attr("cy", d => yScale(d.y))
  .style("fill", "#FFFFFF");

//Character's path   
var locPath = d3.line()
  .x(d => xScale(d.x))
  .y(d => yScale(d.y));
  
locPath.curve(d3.curveCardinal);
  
d3.select("svg")
  .append("path")
  .attr("d", locPath(data))
  .attr("fill", "none")
  .attr("stroke", "#2020FF")
  .attr("stroke-width", 2);

