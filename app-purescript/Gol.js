
"use strict";

exports.logMe = function(x) {
    return function() {
        console.log(x);
    }
}

const window_size = { height: 200, width: 200 }
const margin = { top: 10, bottom: 10, left: 10, right: 10, slider: 50 }

const binom = function(n,k) {
    let x=1;
    let i=1;
    while (i <= k) {
        x = x * (n+1-i) / i;
        i = i + 1;
    }
    return x;
}

exports._binom = binom;

exports.factorial = function(n) {
    let i = 2;
    let x = 1;
    while (i <= n) {
        x = x * i;
        i = i + 1;
    }
    return x;
}

exports._chompPascal = function(q0,n,k0,f) {
    let i = 0;
    let q = q0;
    let k = k0;
    while (k > 0) {
        const x = binom(n+k-1,n-1);
        if (q >= x) {
            i = i+1;
            q = q-x;
            k = k-1;
        } else {
            break;
        }
    }
    return f(i)(q)(k);
}

exports._maxBinom = function(n,lim) {
    let i = 0;
    while (true) {
        if (binom(n+i, i) > lim) {
            return i;
        }
        i = i+1;
    }
}


exports.memoInt = function(f) {
    let table = [];
    return function(x) {
        // console.log(table);
        if (x in table) {
            // console.log("cache hit on " + x);
            return table[x];
        } else {
            // console.log("cache miss on " + x);
            const res = f(x);
            table[x] = res;
            return res;
        }

    }
}

// type Bazaar f a = forall r. (a -> f r) -> f Unit
// type StopBazaar f a = (a -> f Boolean) -> f Unit
exports.testPrint = function(f) {
    let i = 0;
    return f(x => function() {
        if (i > 10) {
            return false;
        } else {
            console.log(x);
            i = i+1;
            return true;
        }
      });
}

// extractor :: c -> (a -> b -> Effect r) -> Effect r
// merger :: r -> r -> r
// bazaar :: (c -> Effect r) -> Effect r
exports._mergeMaps = function(extractor, merger, bazaar) {
    let res = [];
    bazaar(x => function() {
        extractor(x)(i => y => function () {
          if (i in res) {
            res[i] = merger(res[i])(y);
          } else {
            res[i] = y;
          }
        })();
    })();
    return [];
}

exports.trace = function (x) {
    console.log(x);
    return x;
}

exports.initGol1 = function() {
    d3.select("#gol1").selectAll("p").remove();
    const svg = d3.select("#gol1")
        .append("svg")
        .attr("viewBox", [0,0,window_size.width, window_size.height+margin.slider])
        .attr("width","15em")
        .style("margin","auto")
        .style("display","block");
    console.log(svg)
    return svg;
}


// size : { height: Int, width : Int }
// aliveCells : [Thunk [{ x: Int, y: Int, val: Int }]]
exports._drawGol1 = function(svg, size, snapshots) {
    return function() {
        svg.selectAll("*")
            .remove();
        // console.log(cells)
        const cell_size = { height: (window_size.height-margin.top-margin.bottom) / size.height
                          , width: (window_size.width-margin.left-margin.right) / size.width
                          }
        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);
        const drawBoxes = function(cellThunk) {
            grid.selectAll("*").remove();
            drawGrid(grid,size.width,size.height
                    ,window_size.width-margin.left-margin.right
                    ,window_size.height-margin.top-margin.bottom
                    ,cellThunk()
                    );
        }
        const controller =
            setupTimer(svg
                      ,snapshots.length
                      ,window_size.width - margin.left - margin.right
                      ,i => drawBoxes(snapshots[i])
                      )
              .attr("transform", `translate(${margin.left},${window_size.height})`);
    }
}

const drawGrid = function(svg,numx,numy,width,height,cells) {
    const cell_width  = width  / numx;
    const cell_height = height / numy;
    const grid = svg.append("g");
    const max   = d3.max(cells, d=>d.val);
    const color = d3.scaleSequential(d3.interpolateGreens).domain([0,max]);
    const boxes = grid.append("g").selectAll("rect")
      .data(cells)
      .join("rect")
      .attr("width", cell_width)
      .attr("height", cell_height)
      .attr("x", d => d.x*cell_width)
      .attr("y", d => d.y*cell_height)
      .attr("fill",d=>color(d.val))
      .style("opacity",0.8);
    const xlines = grid.append("g").selectAll("line")
                .data(d3.range(numx+1))
                .join("line")
                .attr("stroke-opacity",0.3)
                .attr("stroke","black")
                .attr("stroke-width",0.5)
                .attr("x1",d=>d*cell_width)
                .attr("y1",0)
                .attr("x2",d=>d*cell_width)
                .attr("y2",height);
    const ylines = grid.append("g").selectAll("line")
                .data(d3.range(numy+1))
                .join("line")
                .attr("stroke-opacity",0.3)
                .attr("stroke","black")
                .attr("stroke-width",0.5)
                .attr("y1",d=>d*cell_height)
                .attr("x1",0)
                .attr("y2",d=>d*cell_height)
                .attr("x2",width);
    return grid;
}

const setupTimer = function(svg,size,width,callback) {
    const tAxis = function(g) {
        const sliderino = d3.sliderBottom()
                        .min(0)
                        .max(size-1)
                        .step(1)
                        .width(width-30)
                        .ticks(size)
                        .tickFormat(v => "")
                        .displayFormat(v => "");
        g.call(sliderino);
        return sliderino;
    }
    let timer = null;

    const subslider = svg.append("g")
    const slidercont = subslider.append("g")
            .attr("transform","translate(15,0)");
    const sliderino = tAxis(slidercont.append("g"))
            .on('onchange', callback);
    callback(sliderino.value());
    const button = subslider.append("g");
    const drawButton = function(g,playing,callback) {
        g.select("*").remove();
        g.append("rect")
            .attr("width",50)
            .attr("height",18)
            .attr("rx",2)
            .style("fill","#eee")
            .attr("stroke","#aaa")
            .attr("stroke-width",0.5)
             .style("margin", 0)
             .style("padding", 0)
             .attr("transform", `translate(${width/2-25},20)`);
        const txt = g.append("text")
            .attr("fill","#555")
            .style("text-anchor","middle")
            .attr("pointer-events", "none")
            .attr("font-size", 10)
            .text(playing ? "Pause" : "Play");
        const tbb = txt.node().getBBox();
        txt.attr("transform", `translate(${width/2},${20+18/2+tbb.height/2-2})`);
        g.on("click",null)
            .on("click",callback);
    }

    const play_start = function () {
        button.call(drawButton,true,play_stop);
        play_tick();
        timer = setInterval(play_tick,1000);
    }
    const play_stop = function () {
        button.call(drawButton,false,play_start);
        clearInterval(timer);
        timer = null;
    }
    const play_tick = function () {
        const currval = sliderino.value();
        const nextval = (currval + 1) % size;
        sliderino.value(nextval);
    }
    play_stop();
    return subslider;
}
