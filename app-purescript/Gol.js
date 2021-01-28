
"use strict";

exports.logMe = function(x) {
    return function() {
        console.log(x);
    }
}

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

const ixChomper = function(n,x,f) {
    let k = 0;
    let z = 0;
    while (true) {
        const z2 = binom(n+k,n);
        if (z2 >x) {
            return f(x-z)(k);
        } else {
            k = k+1;
            z = z2;
        }
    }
}

const ixPascal = function(n,x) {
    let y = x;
    let i = 0;
    let res = [];
    while (true) {
        if (i<n) {
            ixChomper(n-i,y,y2 => function(s) {
                y = y2;
                i = i+1;
                res.unshift(s);
            });
        } else {
            return res;
        }
    }
}

exports._ixPascal = ixPascal;

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

const slider_size = { height: 50, width: 200 }

exports.initGolFlat = function(sel) {
    return function () {
        const window_size = { height: 200, width: 200 }
        const margin = { top: 10, bottom: 10, left: 10, right: 10, slider: 50 }
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width, window_size.height+margin.slider])
            .attr("width","15em")
            .style("margin","auto")
            .style("display","block");
        console.log(svg)
        return { svg, window_size, margin } ;
    }
}

// size : { height: Int, width : Int }
// aliveCells : [Thunk [{ x: Int, y: Int, val: Int }]]
exports._drawGolFlat = function({svg, window_size, margin}, size, snapshots) {
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

exports.initGol3D = function(sel) {
    return function () {
        const window_size = { height: 200, width: 200 }
        const maxZ = 6;
        const margin = { top: 10, bottom: 10, left: 10, right: 10, slider: 50 }
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width*(maxZ*2+1), window_size.height+margin.slider])
            .attr("width","100%")
            .style("margin","auto")
            .style("display","block");
        return { svg, window_size, margin, maxZ } ;
    }
}

// size : { height: Int, width : Int }
// aliveCells : [Thunk [{ x: Int, y: Int, zs: [Int] }]]
exports._drawGol3D = function({svg, window_size, margin, maxZ}, size, snapshots) {
    return function() {
        svg.selectAll("*")
            .remove();
        // console.log(cells)
        const cell_size = { height: (window_size.height-margin.top-margin.bottom) / size.height
                          , width: (window_size.width-margin.left-margin.right) / size.width
                          }
        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);
        let splitCache = [];
        const drawBoxes = function(j) {
            grid.selectAll("*").remove();
            let splitCells = [];
            if (j in splitCache) {
                splitCells = splitCache[j];
            } else {
                const cells = snapshots[j]();
                cells.map(function({x,y,zs}) {
                    zs.map(function (z) {
                        if (z in splitCells) {
                            splitCells[z].push({x,y,val:1});
                        } else {
                            splitCells[z] = [{x,y,val:1}];
                        }
                    });
                });
                splitCache[j] = splitCells;
            }
            d3.range(0,maxZ+1).map(function(i) {
                const subgrid = drawGrid(grid,size.width,size.height
                        ,window_size.width-margin.left-margin.right
                        ,window_size.height-margin.top-margin.bottom
                        ,(i in splitCells) ? splitCells[i] : []
                        )
                    .attr("transform",`translate(${(maxZ+i)*window_size.width},0)`);
                if (i > 0) {
                    subgrid.clone(true)
                      .attr("transform",`translate(${(maxZ-i)*window_size.width},0)`);
                }
            });
        }
        const controller =
            setupTimer(svg
                      ,snapshots.length
                      ,window_size.width*3
                      ,drawBoxes
                      )
              .attr("transform", `translate(${window_size.width*(maxZ-1)},${window_size.height})`);
    }
}

exports.initGol4D = function(sel) {
    return function () {
        const window_size = { height: 200, width: 200 }
        const maxZW = 6;
        const margin = { top: 10, bottom: 10, left: 10, right: 10, slider: 50 }
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width*(maxZW*2+1), window_size.height*(maxZW*2+1)+margin.slider])
            .attr("width","100%")
            .style("margin","auto")
            .style("display","block");
        const slidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .attr("width","15em")
                .style("margin","1em auto 1em auto")
                .style("display","block")
                .style("overflow","visible");
        return { svg, slidersvg, window_size, margin, maxZW } ;
    }
}

// size : { height: Int, width : Int }
// aliveCells : [Thunk [{ x: Int, y: Int, zws: [Int}] }]]
exports._drawGol4D = function({svg, slidersvg, window_size, margin, maxZW}, size, snapshots) {
    const maxPascal = binom(2+maxZW,maxZW);
    const symmetries = d3.range(0,maxPascal).map(function(i) {
        const zw = ixPascal(2,i);
        const z0 = zw[0];
        const w0 = zw[1];
        let pos = [{z:z0,w:w0}];
        if (z0 != w0) {
            pos.push({z:w0,w:z0});
        }
        pos.forEach(function({z,w}) {
            if (z > 0) {
                pos.push({z:-z,w:w});
            }
            if (w > 0) {
                pos.push({z:z,w:-w});
            }
            if (z > 0 && w > 0) {
                pos.push({z:-z,w:-w});
            }
        });
        return pos;
    });
    return function() {
        svg.selectAll("*")
            .remove();
        const cell_size = { height: (window_size.height-margin.top-margin.bottom) / size.height
                          , width: (window_size.width-margin.left-margin.right) / size.width
                          }
        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);
        const grids = symmetries.map(function(pos,i) {
            return pos.map(function({z,w}) {
                const res = setupGrid(grid,size.width,size.height
                        ,window_size.width-margin.left-margin.right
                        ,window_size.height-margin.top-margin.bottom
                        ,true
                        );
                res.grid
                  .attr("transform",`translate(${(maxZW+z)*window_size.width},${(maxZW+w)*window_size.height})`);
                return {drawer: res.drawer, eraser: res.eraser};
            });
        });

        let splitCache = [];
        let refresh = [];
        const drawBoxes = function(j) {
            refresh.forEach(eraser => eraser(););
            refresh = [];
            let splitCells = [];
            if (j in splitCache) {
                splitCells = splitCache[j];
            } else {
                const cells = snapshots[j]();
                cells.map(function({x,y,zws}) {
                    zws.map(function (zw) {
                        if (zw in splitCells) {
                            splitCells[zw].push({x,y,val:1});
                        } else {
                            splitCells[zw] = [{x,y,val:1}];
                        }
                    });
                });
                splitCache[j] = splitCells;
            }
            splitCells.forEach(function(ps, zw) {
                grids[zw].forEach(function ({drawer,eraser}) {
                    drawer(ps);
                    refresh.push(eraser);
                });
            });
        }
        const controller =
            setupTimer(slidersvg
                      ,snapshots.length
                      ,slider_size.width
                      ,drawBoxes
                      );
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

const setupGrid = function(svg,numx,numy,width,height,gridlines) {
    const cell_width  = width  / numx;
    const cell_height = height / numy;
    const grid = svg.append("g");
    const boxes = grid.append("g");
    const outline = grid.append("rect")
            .attr("width",width)
            .attr("height",height)
            .attr("fill","none")
            .attr("stroke","black")
            .attr("stroke-width",2)
            .attr("stroke-opacity",0.75);
    if (gridlines) {
        const xlines = grid.append("g").selectAll("line")
                    .data(d3.range(numx-1))
                    .join("line")
                    .attr("stroke-opacity",0.3)
                    .attr("stroke","black")
                    .attr("stroke-width",0.5)
                    .attr("x1",d=>(d+1)*cell_width)
                    .attr("y1",0)
                    .attr("x2",d=>(d+1)*cell_width)
                    .attr("y2",height);
        const ylines = grid.append("g").selectAll("line")
                    .data(d3.range(numy-1))
                    .join("line")
                    .attr("stroke-opacity",0.3)
                    .attr("stroke","black")
                    .attr("stroke-width",0.5)
                    .attr("y1",d=>(d+1)*cell_height)
                    .attr("x1",0)
                    .attr("y2",d=>(d+1)*cell_height)
                    .attr("x2",width);
    }
    const eraser = function() {
        boxes.selectAll("*").remove();
    }
    const drawer = function(cells) {
        const max   = d3.max(cells, d=>d.val);
        const color = d3.scaleSequential(d3.interpolateGreens).domain([0,max]);
        eraser();
        boxes.selectAll("rect")
          .data(cells)
          .join("rect")
          .attr("width", cell_width)
          .attr("height", cell_height)
          .attr("x", d => d.x*cell_width)
          .attr("y", d => d.y*cell_height)
          .attr("fill",d=>color(d.val))
          .style("opacity",0.8);
    }
    return {grid, drawer, eraser};
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
