
"use strict";

const sansSerif = "Helvetica Neue,Helvetica,Arial,sans-serif"

const sameArray = function(xs,ys) {
    const lx = xs.length;
    const ly = ys.length;
    if (lx == ly) {
        for (const i of d3.range(lx)) {
            if (xs[i] != ys[i]) {
                return false;
            }
        }
        return true;
    } else {
        return false;
    }
}

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

const pascalIx = function(ps) {
    let res = 0;
    ps.forEach(function (p, i) {
        if (p > 0) {
            res += binom(p+i,p-1);
        }
    });
    return res;
}

exports.pascalIx = pascalIx;

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
exports._toIntMap = function(extractor, merger, bazaar) {
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
    return res;
}

// merger :: r -> r -> r
// bazaar :: (IntMap r -> Effect r) -> Effect r
exports._unionsIntMap = function(merger, bazaar) {
    let res = [];
    bazaar(xs => function () {
        xs.forEach(function (x, i) {
            if (i in res) {
                res[i] = merger(res[i])(x);
            } else {
                res[i] = x;
            }
        });
    })();
    return res;
}

exports._filterIntMap = function (f, xs) {
    let res = [];
    xs.forEach(function (x, i) {
        if (f(x)) {
            res[i] = x;
        }
    });
    return res;
}

exports.intMapKeys = function (xs) {
    let res = []
    xs.forEach((x, i) => res.push(i));
    return res;
}

exports._singletonIntMap = function(i, x) {
    let res = [];
    res[i] = x;
    return res;
}

exports.trace = function (tr) {
    return function (x) {
        console.log(tr)
        return x;
    }
}

const slider_size = { height: 40, width: 200 }

exports.initGolFlat = function(sel) {
    return function () {
        const window_size = { height: 200, width: 200 }
        const margin = { top: 10, bottom: 10, left: 10, right: 10 }
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width, window_size.height])
            .attr("width","15em")
            .style("margin","auto")
            .style("display","block");
        const dimslidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .attr("width","15em")
                .style("margin","1em auto 0 auto")
                .style("display","block")
                .style("overflow","visible");
        const slidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .attr("width","15em")
                .style("margin","1em auto 0 auto")
                .style("display","block")
                .style("overflow","visible");
        const ptsdisp = d3.select(sel)
                .append("div")
                .style("width","20em")
                .style("height","10em")
                .style("margin","1em auto 0 auto")
                .style("display","block")
                .append("p")
                .style("text-align","center")
                .style("font-size","90%")
                .style("line-height","125%");
        return { svg, slidersvg, dimslidersvg, ptsdisp, window_size, margin } ;
    }
}

// size : { height: Int, width : Int }
// snapshots : [[Thunk [{ x: Int, y: Int, pts: [Int] }]]]     -- top level: time, second level, dim
exports._drawGolFlat = function({svg, slidersvg, dimslidersvg, ptsdisp, window_size, margin}, size, snapshots) {
    const cell_size = { height: (window_size.height-margin.top-margin.bottom) / size.height
                      , width: (window_size.width-margin.left-margin.right) / size.width
                      }
    return function() {
        svg.selectAll("*")
            .remove();
        let currTime = 0;
        let currDim = 0;
        let currSel = undefined;
        let locked = false;
        let ptcache = [];
        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);
        let gridSetup = {};
        const drawBoxes = function(t,d,sel_) {
            const newCells = (t != currTime) || (d != currDim);
            const sel = locked ? currSel : sel_;
            if (newCells) {
                ptcache = [];
                const cells = snapshots[t][d]().map(function({x,y,pts}) {
                    if (x in ptcache) {
                        ptcache[x][y] = pts;
                    } else {
                        ptcache[x] = [];
                        ptcache[x][y] = pts;
                    }
                    return {x,y,val:pts.length};
                });
                gridSetup.drawer(cells);
            }
            if (sel != currSel || newCells) {
                if (sel) {
                    const {x,y} = sel;
                    const pts = (x in ptcache) ? ((y in ptcache[x]) ? ptcache[x][y] : []) : [];
                    if (d > 0) {
                        const ptsstring = (pts.length == 0)
                                        ? "(no points)"
                                        : pts.map(pt => "<" + ixPascal(d,pt).join(",") + ">").join(", ");
                        ptsdisp.text(ptsstring).style("font-style", (pts.length == 0) ? "italic" : "normal");
                    } else {
                        ptsdisp.text((pts.length == 0) ? "(no point)" : "(single point)")
                          .style("font-style", "italic");
                    }
                    const equivGroup = snapshots[t][d]().flatMap(function(testpt) {
                        if (sameArray(testpt.pts, pts)) {
                            return [{x: testpt.x, y: testpt.y, val:1}];
                        } else {
                            return [];
                        }
                    });
                    gridSetup.highlighter(equivGroup);
                } else {
                    gridSetup.highlighter([]);
                    ptsdisp.text("Hover to view points")
                        .style("font-style","italic");
                }
            }
            currTime = t;
            currDim = d;
            currSel = sel;
        }
        gridSetup = setupGrid(grid,"",size.width,size.height
            ,window_size.width-margin.left-margin.right
            ,window_size.height-margin.top-margin.bottom
            , { onmove: ((x,y) => drawBoxes(currTime,currDim,{x,y}))
              , onleave: (() => drawBoxes(currTime,currDim,undefined))
              }
            );
        gridSetup.grid.on("click", function(e) {
            e.preventDefault();
            locked = !locked;
            if (locked) {
                gridSetup.underlighter(0.05);
            } else {
                gridSetup.underlighter(0);
            }
        });
        const dimselbox = dimslidersvg.append("g")
                        .attr("transform","translate(15,0)");
        const dimslider = d3.sliderBottom()
            .min(0)
            .max(snapshots[0].length-1)
            .step(1)
            .width(slider_size.width-30)
            .ticks(snapshots[0].length)
            .tickFormat(v => (v+2)+"")
            .displayFormat(v => "d="+(v+2))
            .on("onchange", d => drawBoxes(currTime,d,currSel));
        dimselbox.call(dimslider);
        dimslider.value(3)

        const controller =
            setupTimer(slidersvg
                      ,snapshots.length
                      ,slider_size.width
                      , i => drawBoxes(i,currDim,currSel)
                      );
    }
}

exports.initGol3D = function(sel) {
    return function () {
        const window_size = { height: 200, width: 200 }
        const maxZ = 6;
        const margin = { top: 10, bottom: 10, left: 10, right: 10 }
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
                .style("margin","1em auto")
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
            clearHighlights.forEach(highlighter => highlighter([]));
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
                const res = setupGrid(grid,"z = " + z,size.width,size.height
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
            refresh.forEach(drawer => drawer([]));
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
        const margin = { top: 10, bottom: 10, left: 10, right: 10 }
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width*(maxZW*2+1), window_size.height*(maxZW*2+1)])
            .attr("width","100%")
            .style("margin","auto")
            .style("display","block");
        const slidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .attr("width","15em")
                .style("margin","1em auto")
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
            clearHighlights.forEach(highlighter => highlighter([]));
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
                const res = setupGrid(grid,"",size.width,size.height
                // const res = setupGrid(grid,z + ", " + w,size.width,size.height
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
            refresh.forEach(drawer => drawer([]));
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

const normalize4d = function({x,y}) {
    const x_ = Math.abs(x);
    const y_ = Math.abs(y);
    return (x_ > y_) ? {x: y_, y: x_} : {x : x_, y : y_};
}

const pascalIx2d = ({x,y}) => pascalIx([x,y]);

const pushOrAdd = function (xs, i, x) {
    if (i in xs) {
        xs[i].push(x);
    } else {
        xs[i] = [x];
    }
}

const compressVals = function(ps) {
    let xs = [];
    ps.forEach(function({x,y,val}) {
        if (x in xs) {
            if (!(y in xs[x])) {
                xs[x][y] = 0;
            }
        } else {
            xs[x] = [];
            xs[x][y] = 0;
        }
        xs[x][y] += val;
    });
    let out = [];
    xs.forEach(function(ys, x) {
        ys.forEach(function(val, y) {
            out.push({x,y,val});
        });
    });
    return out;
}

const neighbsArray = function(maxZW, reversed) {
    const maxPascal = binom(2+maxZW,maxZW);
    let res = d3.range(0,maxPascal).map( () => ({ neighbBox: [], neighbHighlight: [] }) );
    d3.range(0,maxPascal).forEach(function(i) {
        const pos = ixPascal(2, i);
        neighbs2d.slice(1).forEach(function(dp) {
            const npos = {x: pos[0]+dp.x, y: pos[1]+dp.y};
            const j = pascalIx2d(normalize4d(npos));
            const newPt = {x:dp.x+1, y:(-dp.y)+1, val:1};
            if (j < maxPascal) {
                if (reversed) {
                    pushOrAdd(res[j].neighbHighlight, j, newPt);
                    pushOrAdd(res[j].neighbBox, i, newPt);
                } else {
                    pushOrAdd(res[i].neighbBox, j, newPt);
                    pushOrAdd(res[i].neighbHighlight, i, newPt);
                }
            }
        });
    });
    return res.map(({neighbBox, neighbHighlight}) =>
                        ({ neighbBox: neighbBox.map(compressVals)
                        , neighbHighlight: neighbHighlight.map(compressVals)
                        })
                    );
}

// getNeighbs :: Int -> [{x: Int, weight: Int}]
exports._drawGolSyms = function(sel, reversed) {
    const window_size = { height: 200, width: 200 }
    const maxZW = 6;
    const margin = { top: 10, bottom: 10, left: 10, right: 10 };

    const maxPascal = binom(2+maxZW,maxZW);

    const neighbs = neighbsArray(maxZW, reversed);

    return function () {
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0,window_size.width*(maxZW+1), window_size.height*(maxZW+1)])
                .attr("width","20em")
                .style("margin","auto")
                .style("overflow","visible")
                .style("display","block");

        const allBoxes = svg.append("g");
        let boxes = [];
        let clearHighlights = [];
        const clearAllHighlights = function () {
            clearHighlights.forEach(clr => clr());
            clearHighlights = [];
        }
        const highlightNeighbs = function (i) {
            clearAllHighlights();
            neighbs[i].neighbBox.forEach(function(ps, j) {
                boxes[j].drawer(ps);
                boxes[j].settext(ps.length + "");
                boxes[j].underlighter(ps.length/8);
                clearHighlights.push(() => boxes[j].drawer([]));
                clearHighlights.push(() => boxes[j].settext(""));
                clearHighlights.push(() => boxes[j].underlighter(0));
            });
            neighbs[i].neighbHighlight.forEach(function(ps,j) {
                boxes[j].highlighter(ps);
                clearHighlights.push(() => boxes[j].highlighter([]));
            });
            if (!(i in neighbs[i].neighbBox)) {
                boxes[i].settext("0");
                clearHighlights.push(() => boxes[i].settext(""));
            }
        }
        boxes = d3.range(0,maxPascal).map(function (i) {
            const zw = ixPascal(2,i);
            const z0 = zw[0];
            const w0 = zw[1];
            const res =
                setupGrid( allBoxes
                         , z0 + ", " + w0
                         , 3, 3
                         , window_size.width-margin.left-margin.right
                         , window_size.height-margin.top-margin.bottom
                         , { onmove: function() {
                               highlightNeighbs(i);
                             }
                           , onleave: clearAllHighlights
                           }
                         );
            res.grid
              .attr("transform",`translate(${window_size.width*z0+margin.left},${window_size.height*(maxZW-w0)+margin.top})`);
            return { drawer: res.drawer
                   , highlighter: res.highlighter
                   , settext: res.settext
                   , underlighter: res.underlighter
                   };
        });

        return svg;
    }
}

const setupGrid = function(svg,label,numx,numy,width,height,handlers) {
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
    const lab = grid.append("text")
            .attr("fill","#333")
            .style("text-anchor","middle")
            .attr("pointer-events","none")
            .attr("font-size",(width+height)/10)
            .attr("font-family",sansSerif)
            .attr("transform", `translate(${width/2},${height*9/10})`)
            .attr("opacity",0.85)
            .text(label);
    const hightext = grid.append("text")
            .attr("fill","#333")
            .style("text-anchor","middle")
            .attr("pointer-events","none")
            .attr("font-size",(width+height)/6)
            .attr("font-family",sansSerif)
            .attr("transform", `translate(${width/2},${height*(2/3-1/20)})`)
            .attr("opacity",0.85)
            .text("");
    const capture = grid.append("rect")
            .attr("width",width)
            .attr("height",height)
            .attr("fill","white")
            .attr("opacity",0);
    const mover = function(e) {
        e.preventDefault();
        const [mx,my] = d3.pointer(e,this);
        const x = Math.floor(mx/cell_width);
        const y = Math.floor(my/cell_height);
        if (x >= 0 && x < numx && y >= 0 && y < numy) {
          if ("onmove" in handlers) {
            handlers.onmove(x,y);
          }
        }
    }
    const leaver = function(e) {
        if ("onleave" in handlers) {
            handlers.onleave();
        }
    }
    // capture.style("-webkit-tap-highlight-color", "transparent")
    capture
          .on("touchmove", mover)
          .on("touchstart", mover)
          .on("touchend", leaver)
          .on("mousemove", mover)
          .on("mouseenter", mover)
          .on("mouseleave", leaver);
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
           , settext: function (t) {
               hightext.text(t);
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

// f :: a -> [a]
exports._buildHierarchy = function (x,f) {
    return d3.hierarchy(x,f);
}

const expandRun = r => r.flatMap((d, i) => d3.range(d).map(() => i))

// type Mult = { total :: Int, here :: String }
// type Contrib = { left :: Maybe Int
//                , here :: Maybe Int
//                , chosen :: Array Int
//                , leftovers :: Array Int
//                , multP :: Lazy Mult
//                , multQ :: Lazy Mult
//                , allSame :: Boolean
//                }
//
// forward :: Boolean
// getContrib :: Node -> Lazy Contrib
// vecRun :: Int -> Int -> [Int]
// mkHier :: Int -> Int -> Hierarchy
exports._drawTree = function(sel,forward,vecRun,mkHier,getContrib) {
    const margin = { top: 10, bottom: 10, left: 30, right: 42 };
    const maxZ = 3;

    const expandChosens = function(c) {
        const contlen = c.leftovers.length;
        const choselen = c.chosen.length;
        const pad = d3.range(contlen-choselen).map(() => 0);
        const fullchose = pad.concat(c.chosen);
        const expandLeft = expandRun(c.leftovers);
        const prefix = (expandLeft.length > 0) ? ("[" + expandLeft.join(",") + "]") : "";
        const suffix = expandRun(fullchose).join(",");
        const infix = (prefix.length > 0 && suffix.length > 0) ? "," : "";
        return "<" + prefix + infix + suffix + ">";
    }

    return function () {
        d3.select(sel).selectAll("p").remove();
        const dimslidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .attr("width","15em")
                .style("margin","0.25em auto 0 auto")
                .style("display","block")
                .style("overflow","visible");
        const selbox = d3.select(sel)
                        .append("form")
                        .style("width","15em")
                        .style("margin","0 auto 0 auto")
                        .style("display","block")
                        .append("select")
                        .style("width","100%");
        const svg = d3.select(sel)
                .append("svg")
                .attr("width","100%")
                .style("margin","auto")
                .style("overflow","visible")
                .style("display","block");

        const setupSelect = function(dim) {
            selbox.selectAll("*").remove();
            const numOpts = binom(dim+maxZ, maxZ);
            const ptOpts = d3.range(numOpts).map(function (d) {
                const vr = vecRun(dim)(d).slice(0,-1);
                const pt = ixPascal(dim,d);
                return { pt: d
                       , disp: "<" + pt.join(",") + "> (" + vr.join("-") + ")"
                       }

            });

            selbox.selectAll("option")
                  .data(ptOpts)
                  .join("option")
                  .attr("value", d => d.pt)
                  .text(d => d.disp);
        }

        const dimselbox = dimslidersvg.append("g")
                        .attr("transform","translate(15,0)");
        const dimslider = d3.sliderBottom()
            .min(1)
            .max(5)
            .step(1)
            .width(slider_size.width-30)
            .ticks(5)
            .tickFormat(v => (v+2)+"")
            .displayFormat(v => "d="+(v+2));
        dimselbox.call(dimslider);

        const drawTree = function(pt) {
            svg.selectAll("*")
                .remove();
            const dim = dimslider.value()
            const hier = mkHier(dim)(pt);
            const numchild = hier.leaves().length;
            // console.log(numchild);
            // window["testhier"] = hier;
            const window_size = { height: numchild*15+margin.top+margin.bottom, width: 400 }
            const tree = d3.tree()
                .size([ window_size.height-margin.top-margin.bottom
                      , window_size.width-margin.left-margin.right
                      ])(hier);
            svg.attr("viewBox", [0,0,window_size.width,window_size.height]);
            selbox.property("value",pt);

            const treecont = svg.append("g")
                     .attr("transform",`translate(${margin.left},${margin.top})`);

            const link = treecont.append("g");
            link.selectAll("path")
                    .data(tree.links())
                    .join("path")
                    .attr("fill","none")
                    .attr("stroke","#555")
                    .attr("stroke-opacity",0.4)
                    .attr("stroke-width",1)
                    .attr("d", d3.linkHorizontal()
                                    .x(d => d.y)
                                    .y(d => d.x)
                         );

            const node = treecont.append("g")
                    .attr("stroke-linejoin", "round")
                    .attr("stroke-width", 3)
                    .selectAll("g")
                    .data(tree.descendants())
                    .join("g")
                    .attr("transform", d => `translate(${d.y},${d.x})`);

            const tooltip = treecont.append("g")
                               .style("opacity",0.95)
                               .attr("display","none");

            tooltip.append("rect")
                .attr("width",40)
                .attr("height",12)
                .attr("x",-20)
                .attr("y",6)
                .style("fill","#fff")
                .style("stroke","#666")
                .style("stroke-width","0.5px");

            const tttext = tooltip.append("text")
                .attr("text-anchor", "middle")
                .style("font-size",6.5)
                .attr("font-family",sansSerif)
                .attr("x",0)
                .attr("y",14);

            const highlightNodes = function(d) {
                const c = getContrib(d.data)();
                const targChosen = c.chosen;
                node.selectAll("circle")
                    .filter(d => sameArray(targChosen,getContrib(d.data)().chosen))
                    .attr("r",4)
                    .attr("fill","blue")
                    .attr("opacity","0.5");

                const ancs = d.ancestors();
                const checkAnc = function(q) {
                    for (const a of ancs) {
                        if (a == q) {
                            return true;
                        }
                    }
                    return false;
                }

                link.selectAll("path")
                    .filter(d => checkAnc(d.target))
                    .attr("stroke-opacity",0.9);

                tooltip.attr("display",null)
                        .attr("transform",`translate(${d.y},${d.x})`);
                tttext.text(expandChosens(c));
            }
            const clearNodes = function() {
                node.selectAll("circle")
                    .attr("r",d => (("children" in d) && d.parent) ? 1.5 : 2)
                    .attr("fill",d => (("children" in d) && d.parent) ? "#333" : "red")
                    .attr("opacity","0.75");
                link.selectAll("path")
                    .attr("stroke-opacity",0.4);

                tooltip.attr("display","none");
            }

            node.append("circle")
                    .attr("opacity",0.75)
                    .on("mouseenter", (e,d) => highlightNodes(d))
                    .on("touchstart", (e,d) => highlightNodes(d))
                    .on("mouseleave", () => clearNodes())
                    .on("touchend", () => clearNodes());
            clearNodes();

            node.append("text")
                .attr("text-anchor", "end")
                .attr("dy",-1.5)
                .attr("x",-4)
                .attr("opacity", d => ("children" in d) ? 0.9 : 0.6)
                .text(d => getContrib(d.data)().leftovers.join("-"));

            node.append("text")
                .attr("text-anchor", "end")
                .attr("dy",4.5)
                .attr("x",-4)
                .attr("opacity", d => (d.parent) ? 0.9 : 0.6)
                .text(function (d) {
                        const c = getContrib(d.data)();
                        const contlen = c.leftovers.length;
                        const choselen = c.chosen.length;
                        const pad = d3.range(contlen-choselen).map(() => "_");
                        return pad.concat(c.chosen).join("-");
                     })
                .style("font-weight", d => ("children" in d) ? "normal" : "bold")
                .style("text-decoration", d => ("children" in d) ? "none" : (getContrib(d.data)().allSame ? "line-through" : "none"));

            node.append("text")
                .attr("text-anchor", "start")
                .attr("dy",2)
                .attr("x",3)
                .attr("opacity",0.9)
                .text(function (d) {
                        const c = getContrib(d.data)();
                        const m = forward ? c.multQ() : c.multP();
                        if ("children" in d) {
                            return m.here;
                        } else {
                            const endMult = c.allSame ? 0 : m.total;
                            return m.here + " → " + endMult;
                        }

                    })
                .style("text-decoration", d => ("children" in d) ? "none" : (getContrib(d.data)().allSame ? "line-through" : "none"));

            node.selectAll("text")
                .attr("font-family",sansSerif)
                .style("font-size",6)
               .clone(true).lower()
               .attr("stroke", "white");

        }

        dimslider.on("onchange", d => { setupSelect(d); drawTree(0); } );
        selbox.on("change", d => drawTree(d.srcElement.value));
        dimslider.value(4);
        drawTree(22);

        return svg;
    }
}


exports.undefined = 0;
exports._assignWindow = function(p,x) { return function () { window[p] = x; } }
