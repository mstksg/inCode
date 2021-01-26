
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
        const boxes = svg.append("g");
        const drawBoxes = function(cellThunk) {
            const cells = cellThunk();
            const max   = d3.max(cells, d=>d.val);
            const color = d3.scaleSequential(d3.interpolateGreens).domain([0,max]);
            boxes.selectAll("*").remove();
            boxes.selectAll("rect")
              .data(cells)
              .join("rect")
              .attr("width", cell_size.width)
              .attr("height", cell_size.height)
              .attr("x", d => d.x*cell_size.width + margin.left)
              .attr("y", d => d.y*cell_size.height + margin.top)
              .attr("fill",d=>color(d.val))
              .style("opacity",0.8);

        }
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
        const tAxis = function(g) {
            const sliderino = d3.sliderBottom()
                            .min(0)
                            .max(snapshots.length-1)
                            .step(1)
                            .width(window_size.width - margin.left - margin.left - margin.right - margin.right)
                            .ticks(snapshots.length)
                            .tickFormat(v => "")
                            .displayFormat(v => "");
            g.attr("transform", `translate(${margin.left + margin.left},${window_size.height})`)
                .call(sliderino);
            return sliderino;
        }
        let timer = null;

        const subslider = svg.append("g");
        const sliderino = tAxis(subslider.append("g"))
                .on('onchange', v => drawBoxes(snapshots[v]));
        drawBoxes(snapshots[sliderino.value()]);

        const button = svg.append("g");
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
                 .attr("transform", `translate(${window_size.width/2-25},${window_size.height+margin.slider-30})`);
            g.append("text")
                .attr("fill","#555")
                .style("text-anchor","middle")
                .attr("pointer-events", "none")
                .attr("font-size", 10)
                .attr("transform", `translate(${window_size.width/2},${window_size.height+margin.slider-18})`)
                .text(playing ? "Pause" : "Play");
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
            const nextval = (currval + 1) % snapshots.length;
            sliderino.value(nextval);
        }

        play_stop();


    }
}
