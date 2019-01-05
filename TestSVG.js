// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

var barHeight = Math.ceil(height / data.length);


var margin = {top: 20, right: 10, bottom: 20, left: 10};
var svg = d3.select("body").append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
    .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

d3.xml("images/IslaNublar600.svg","image/svg+xml", function(xml) {
  
  var anno = svg.selectAll("anno")
  		.data("1")
  		.enter()
  		.append("g")
  		.attr("class","anno")
  		.attr("transform", function(d,i) {
  			return "scale(0.5)";
  		});

  var importedNode = document.importNode(xml.documentElement, true);
  
  d3.select(".anno")[0][0].appendChild(importedNode.cloneNode(true));
});

