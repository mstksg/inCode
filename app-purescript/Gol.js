
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
            .attr("viewBox", [0,0,window_size.width, window_size.height])
            .attr("width","15em")
            .style("margin","auto")
            .style("display","block");
        const slidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .attr("width","15em")
                .style("margin","0.5em auto")
                .style("display","block")
                .style("overflow","visible");
        return { svg, slidersvg, window_size, margin } ;
    }
}

// size : { height: Int, width : Int }
// aliveCells : [Thunk [{ x: Int, y: Int, val: Int }]]
exports._drawGolFlat = function({svg, slidersvg, window_size, margin}, size, snapshots) {
    const cell_size = { height: (window_size.height-margin.top-margin.bottom) / size.height
                      , width: (window_size.width-margin.left-margin.right) / size.width
                      }
    return function() {
        svg.selectAll("*")
            .remove();
        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);
        const gridSetup = setupGrid(grid,size.width,size.height
            ,window_size.width-margin.left-margin.right
            ,window_size.height-margin.top-margin.bottom
            ,{}
            );
        const drawBoxes = function(i) {
            gridSetup.drawer(snapshots[i]());
        }
        const controller =
            setupTimer(slidersvg
                      ,snapshots.length
                      ,slider_size.width
                      ,drawBoxes
                      );
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
            .attr("viewBox", [0,0,window_size.width*(maxZ*2+1), window_size.height])
            .attr("width","100%")
            .style("margin","auto")
            .style("display","block");
        const slidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .attr("width","15em")
                .style("margin","0.5em auto")
                .style("display","block")
                .style("overflow","visible");
        return { svg, slidersvg, window_size, margin, maxZ } ;
    }
}

// size : { height: Int, width : Int }
// aliveCells : [Thunk [{ x: Int, y: Int, zs: [Int] }]]
exports._drawGol3D = function({svg, slidersvg, window_size, margin, maxZ}, size, snapshots) {
    const symmetries = d3.range(0,maxZ+1).map(z => (z > 0) ? [-z,z] : [z]);
    const cell_size = { height: (window_size.height-margin.top-margin.bottom) / size.height
                      , width: (window_size.width-margin.left-margin.right) / size.width
                      }
    return function() {
        svg.selectAll("*")
            .remove();
        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);
        let fullGrid = []
        let clearHighlights = [];
        let grids = [];
        const clearAllHighlights = function() {
            clearHighlights.forEach(highlighter => highlighter());
            clearHighlights = [];
        }
        const highlightNeighbs = function(x,y,z,i) {
            clearAllHighlights();
            [-1,0,1].forEach(function (dz) {
                if (inRange(-maxZ,maxZ+1,z+dz)) {
                    const hltr = fullGrid[z+dz+maxZ].highlighter
                    const baseNeighbs = (dz == 0)
                                      ? neighbs2d.slice(1)
                                      : neighbs2d;
                    hltr(
                        baseNeighbs
                            .map(dxy => ({x:x+dxy.x,y:y+dxy.y,val:1}))
                            .filter(xy => (inRange(0,size.width,xy.x) && inRange(0,size.height,xy.y)))
                    );
                    clearHighlights.push(() => hltr([]));
                }
            });
            grids[i].forEach(function (g) {
                g.underlighter(0.25);
                clearHighlights.push(() => g.underlighter(0));
            });
        }
        grids = symmetries.map(function(pos,i) {
            return pos.map(function(z) {
                const res = setupGrid(grid,size.width,size.height
                        ,window_size.width-margin.left-margin.right
                        ,window_size.height-margin.top-margin.bottom
                        , { onmove: function(x,y) {
                              highlightNeighbs(x,y,z,i);
                            }
                          , onleave: clearAllHighlights
                          }
                        );
                res.grid
                  .attr("transform",`translate(${(maxZ+z)*window_size.width},0)`);
                fullGrid[z+maxZ] = {drawer: res.drawer, highlighter: res.highlighter, underlighter: res.underlighter};
                return {drawer: res.drawer, highlighter: res.highlighter, underlighter: res.underlighter};
            });
        });
        let splitCache = [];
        let refresh = [];
        const drawBoxes = function(j) {
            refresh.forEach(drawer => drawer([]););
            refresh = [];
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
            splitCells.forEach(function(ps, z) {
                grids[z].forEach(function ({drawer}) {
                    drawer(ps);
                    refresh.push(drawer);
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
                .style("margin","0.5em auto")
                .style("display","block")
                .style("overflow","visible");
        return { svg, slidersvg, window_size, margin, maxZW } ;
    }
}

const neighbs2d =
    [{x:0,y:0},{x:0,y:-1},{x:0,y:1}
    ,{x:-1,y:0},{x:-1,y:-1},{x:-1,y:1}
    ,{x:1,y:0},{x:1,y:-1},{x:1,y:1}
    ]

const inRange = function(mn,mx,x) {
    return (x >= mn && x < mx);
}

// size : { height: Int, width : Int }
// aliveCells : [Thunk [{ x: Int, y: Int, zws: [Int}] }]]
// neighbs : Int -> [Int]
exports._drawGol4D = function({svg, slidersvg, window_size, margin, maxZW}, size, neighbs, snapshots) {
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
    const cell_size = { height: (window_size.height-margin.top-margin.bottom) / size.height
                      , width: (window_size.width-margin.left-margin.right) / size.width
                      }
    return function() {
        svg.selectAll("*")
            .remove();
        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);

        let fullGrid = d3.range(2*maxZW+1).map(c => []);
        let clearHighlights = [];
        let grids = [];
        const clearAllHighlights = function() {
            clearHighlights.forEach(highlighter => highlighter());
            clearHighlights = [];
        }
        const highlightNeighbs = function(x,y,z,w,i) {
            clearAllHighlights();
            neighbs2d.forEach(function (dzw) {
                if (inRange(-maxZW,maxZW+1,z+dzw.x) && inRange(-maxZW,maxZW+1,w+dzw.y)) {
                    const hltr = fullGrid[z+dzw.x+maxZW][w+dzw.y+maxZW].highlighter
                    // hltr([{x,y,val:1}])
                    const baseNeighbs = (dzw.x == 0 && dzw.y == 0)
                                      ? neighbs2d.slice(1)
                                      : neighbs2d;
                    hltr(
                        baseNeighbs
                            .map(dxy => ({x:x+dxy.x,y:y+dxy.y,val:1}))
                            .filter(xy => (inRange(0,size.width,xy.x) && inRange(0,size.height,xy.y)))
                    );
                    clearHighlights.push(() => hltr([]));
                }
            });
            grids[i].forEach(function (g) {
                g.underlighter(0.25);
                clearHighlights.push(() => g.underlighter(0));
            });
        }
        grids = symmetries.map(function(pos,i) {
            return pos.map(function({z,w}) {
                const res = setupGrid(grid,size.width,size.height
                        ,window_size.width-margin.left-margin.right
                        ,window_size.height-margin.top-margin.bottom
                        , { onmove: function(x,y) {
                              highlightNeighbs(x,y,z,w,i);
                            }
                          , onleave: clearAllHighlights
                          }
                        );
                res.grid
                  .attr("transform",`translate(${(maxZW+z)*window_size.width},${(maxZW+w)*window_size.height})`);
                fullGrid[z+maxZW][w+maxZW] = {drawer: res.drawer, highlighter: res.highlighter, underlighter: res.underlighter};
                return {drawer: res.drawer, highlighter: res.highlighter, underlighter: res.underlighter};
            });
        });

        let splitCache = [];
        let refresh = [];
        const drawBoxes = function(j) {
            refresh.forEach(drawer => drawer([]););
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
                grids[zw].forEach(function ({drawer}) {
                    drawer(ps);
                    refresh.push(drawer);
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

const setupGrid = function(svg,numx,numy,width,height, handlers) {
    const cell_width  = width  / numx;
    const cell_height = height / numy;
    const grid = svg.append("g");
    const underlight = grid.append("rect")
            .attr("width",width)
            .attr("height",height)
            .attr("fill","yellow")
            .attr("opacity",0);
    const outline = grid.append("rect")
            .attr("width",width)
            .attr("height",height)
            .attr("fill","none")
            .attr("stroke","black")
            .attr("stroke-width",2)
            .attr("stroke-opacity",0.75);
    const boxes = grid.append("g");
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
    const highlights = grid.append("g");
    const highlights2 = grid.append("g");
    const capture = grid.append("rect")
            .attr("width",width)
            .attr("height",height)
            .attr("fill","white")
            .attr("opacity",0);
    capture.on("mousemove", function (e) {
        e.preventDefault();
        const [mx,my] = d3.pointer(e,this);
        const x = Math.floor(mx/cell_width);
        const y = Math.floor(my/cell_height);
        if (x >= 0 && x < numx && y >= 0 && y < numy) {
          if ("onmove" in handlers) {
            handlers.onmove(x,y);
          }
        }
    });
    capture.on("mouseleave", function (e) {
        if ("onleave" in handlers) {
            handlers.onleave();
        }
    });
    const mkDrawer = function(s, col, o) {
        return function(cells) {
            const max   = d3.max(cells, d=>d.val);
            const color = d3.scaleSequential(col).domain([0,max]);
            s.selectAll("*").remove();
            s.selectAll("rect")
              .data(cells)
              .join("rect")
              .attr("width", cell_width)
              .attr("height", cell_height)
              .attr("x", d => d.x*cell_width)
              .attr("y", d => d.y*cell_height)
              .attr("fill",d=>color(d.val))
              .style("opacity",o);
        }
    }
    return { grid
           , drawer: mkDrawer(boxes,d3.interpolateGreens,0.8)
           , highlighter: mkDrawer(highlights,d3.interpolateBlues,0.3)
           , underlighter: function(o) {
               underlight.attr("opacity", o);
             }
           };
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

// returns [{ a: Int, b : Int }], number inside number, single item, empty if
// nothing
const getSquareIx = function(width,margin,cells,x) {
    const a = Math.floor(x/width);
    const cellWidth = (width - 2 * margin) / cells;
    const y = (x % width) - margin;
    if (y < 0) {
        return [];
    } else {
        const b = y % cellWidth;
        if (b < cells) {
            return [{a, b}];
        } else {
            return [];
        }
    }
}
