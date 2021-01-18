
"use strict";

exports.logMe = function(x) {
    return function() {
        console.log(x);
    }
}

const window_size = { height: 200, width: 200 }
const margin = { top: 5, bottom: 5, left: 5, right: 5 }

exports.initGol1 = function() {
    d3.select("#gol1").selectAll("p").remove();
    const svg = d3.select("#gol1")
        .append("svg")
        .attr("viewBox", [0,0,window_size.width, window_size.height])
        .attr("height","15em")
        .style("margin","auto")
        .style("display","block");
    console.log(svg)
    return svg;
}

// aliveCells : [{ x: Int, y: Int, val: Int }]
// size : { height: Int, width : Int }
exports._drawGol1 = function(svg, size, cells) {
    return function() {
        svg.selectAll("*")
            .remove();
        console.log(cells)
        const cell_size = { height: (window_size.height-margin.top-margin.bottom) / size.height
                          , width: (window_size.width-margin.left-margin.right) / size.width
                          }
        const max   = d3.max(cells, d=>d.val);
        const color = d3.scaleSequential(d3.interpolateGreens).domain([0,max]);
        const boxes = svg.append("g").selectAll("rect")
            .data(cells)
            .join("rect")
            .attr("width", cell_size.width)
            .attr("height", cell_size.height)
            .attr("x", d => d.x*cell_size.width + margin.left)
            .attr("y", d => d.y*cell_size.height + margin.top)
            .attr("fill",d=>color(d.val))
            .style("opacity",0.8);
        const xlines = svg.append("g").selectAll("line")
                    .data(d3.range(size.width+1))
                    .join("line")
                    .attr("stroke-opacity",0.3)
                    .attr("stroke","black")
                    .attr("stroke-width",0.5)
                    .attr("x1",d=>d*cell_size.width + margin.left)
                    .attr("y1",margin.top)
                    .attr("x2",d=>d*cell_size.width + margin.left)
                    .attr("y2",window_size.height-margin.bottom);
        const ylines = svg.append("g").selectAll("line")
                    .data(d3.range(size.height+1))
                    .join("line")
                    .attr("stroke-opacity",0.3)
                    .attr("stroke","black")
                    .attr("stroke-width",0.5)
                    .attr("y1",d=>d*cell_size.height + margin.top)
                    .attr("x1",margin.left)
                    .attr("y2",d=>d*cell_size.height + margin.top)
                    .attr("x2",window_size.width-margin.right);
    }
}
