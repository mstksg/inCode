
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

const maxBinom = function(n,lim) {
    let i = 0;
    while (true) {
        if (binom(n+i, i) > lim) {
            return i;
        }
        i = i+1;
    }
};

exports._maxBinom = maxBinom;

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

const holeyForEach = function(xs,f) {
    for (const i in xs) {
        f(i,xs[i]);
    }
}

// merger :: r -> r -> r
// bazaar :: (IntMap r -> Effect r) -> Effect r
exports._unionsIntMap = function(merger, bazaar) {
    let res = [];
    bazaar(xs => function () {
        holeyForEach(xs,function (i,x) {
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
    holeyForEach(xs, function(i,x) {
        if (f(x)) {
            res[i] = x;
        }
    });
    return res;
}

exports.intMapKeys = function (xs) {
    let res = []
    holeyForEach(xs, (i, x) => res.push(i));
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

const MODE = { OFF: 0, CLEAR: 1, SET: 2 }

// dispPts :: Array [{x:Int, y:Int}] -> String
// callback :: (Array [{x:Int, y:Int}] -> Effect Unit)
exports._setupDrawer = function(sel, size, dispPts, callback) {
    const window_size = { height: 200, width: 200 }
    const margin = { top: 10, bottom: 10, left: 10, right: 10 }
    return function() {
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width, window_size.height])
            .style("max-width","15em")
            .style("margin","auto")
            .style("display","block");

        const copyForm = d3.select(sel)
                        .append("form")
                        .style("max-width","15em")
                        .style("margin","1em auto 0 auto")
                        .style("display","block")
                        .append("input")
                        .attr("id","copyblock")
                        .style("padding","0.25rem 0.5rem")
                        .style("font-size","90%")
                        .style("width","100%")
                        .style("border","1px solid #555")
                        .style("border-radius","3px");
        const helpMsg = "Use grid to draw. Click on URL to copy permalink."
        const helpdisp = d3.select(sel)
                .append("div")
                .style("max-width","15em")
                .style("height","2em")
                .style("margin","1em auto 0 auto")
                .style("display","block")
                .append("p")
                .style("text-align","center")
                .style("font-style","italic")
                .style("font-size","85%")
                .style("line-height","133%")
                .text(helpMsg);

        copyForm.on("click", function(e) {
            const esel = e.srcElement;
            esel.select()
            esel.setSelectionRange(0,999999);
            document.execCommand("copy");
            helpdisp.text("URL copied!")
            setTimeout(function () {
                    esel.setSelectionRange(0,0);
                    helpdisp.text(helpMsg);
                }, 1000
            );
        })

        let activePts = []

        let currx = null;
        let curry = null;
        let grid = {};

        const mkOutput = function() {
            return activePts.flatMap((ys,x) =>
                ys.flatMap((b, y) =>
                  ((b) ? [{x,y,val:1}] : [])
              ));
        }

        const drawGrid = function() {
            grid.drawer(mkOutput());
            // callback(output.map(({x,y}) => ({x,y})))();
        }

        const setCopy = function () {
            copyForm.attr("value",
                      window.location.origin
                    + window.location.pathname
                    + "?points=" + dispPts(mkOutput().map(({x,y})=>({x,y})))
              );
        }

        const setGrid = function(xs) {
            activePts =
                d3.range(size.width)
                  .map( () => d3.range(size.height).map( () => false) );
            if (xs) {
                xs.forEach(function({x,y}) {
                    activePts[x][y] = true;
                });
            }
            setCopy();
            drawGrid();
        }

        let mode = MODE.OFF;
        const drawer = function (x,y) {
            currx = x;
            curry = y;
            if (mode > MODE.OFF) {
                if (!(x == null || y == null)) {
                    const oldset = activePts[x][y];
                    const newset = (mode == MODE.SET);
                    activePts[x][y] = newset;

                    if (oldset != newset) {
                        drawGrid();
                    }
                }
            }
        }

        const gridbox = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);
        grid = setupGrid(gridbox,"",size.width,size.height
            ,window_size.width-margin.left-margin.right
            ,window_size.height-margin.top-margin.bottom
            , { onmove: ((x,y) => drawer(x,y))
              , onleave: (function () { mode = MODE.OFF; })
              }
            );
        const clickon = function(e) {
            e.preventDefault();
            // console.log(currx,curry);
            if (!(currx == null || curry == null)) {
                mode = (activePts[currx][curry]) ? MODE.CLEAR : MODE.SET;
                // console.log(activePts[currx][curry],mode);
            } else {
                // console.log(e);
                mode = MODE.SET;
            }
            drawer(currx,curry);
        }
        const clickoff = function(e) {
            e.preventDefault();
            mode = MODE.OFF;
            setCopy();
            callback(mkOutput().map(({x,y}) => ({x,y})))();
        }
        grid.grid.on("mousedown",clickon)
           .on("touchstart",clickon)
           .on("mouseup",clickoff)
           .on("touchend",clickoff);
        setGrid([]);

        return function (xs) {
            return function () {
                setGrid(xs);
                callback(mkOutput().map(({x,y}) => ({x,y})))();
            }
        }
    }
}

const slider_size = { height: 40, width: 200 }

const neighbs2d =
    [{x:0,y:0},{x:0,y:-1},{x:0,y:1}
    ,{x:-1,y:0},{x:-1,y:-1},{x:-1,y:1}
    ,{x:1,y:0},{x:1,y:-1},{x:1,y:1}
    ]

const inRange = function(mn,mx,x) {
    return (x >= mn && x < mx);
}


// snapshots : [[Thunk [{ x: Int, y: Int, pts: [Int] }]]]     -- top level: time, second level, dim
// if maxDim is null, only show d=0
// showPts: include that extra div on the bottom
exports._setupGolFlat = function(sel,showPts,{height,width,maxT,maxDim}) {
    const window_size = { height: 200, width: 200 }
    const margin = { top: 10, bottom: 10, left: 10, right: 10 }
    const cell_size = { height: (window_size.height-margin.top-margin.bottom) / height
                      , width: (window_size.width-margin.left-margin.right) / width
                      }
    return function () {
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width, window_size.height])
            .style("max-width","15em")
            .style("margin","auto")
            .style("display","block");
        let dimslidersvg = null;
        if (maxDim) {
            dimslidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .style("max-width","15em")
                .style("margin","1em auto 0 auto")
                .style("display","block")
                .style("overflow","visible");
        }
        const slidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .style("max-width","15em")
                .style("margin","1em auto 0 auto")
                .style("display","block")
                .style("overflow","visible");
        let ptsdisp = null;
        if (showPts) {
            ptsdisp = d3.select(sel)
                .append("div")
                .style("max-width","20em")
                .style("height","10em")
                .style("margin","1em auto 0 auto")
                .style("display","block")
                .append("p")
                .style("text-align","center")
                .style("font-size","90%")
                .style("line-height","125%");
        }
        let currTime = 0;
        let currDim = 0;
        let currSel = undefined;
        let locked = false;
        let ptcache = [];
        let snapshots = d3.range(maxDim+1).map(() => d3.range(maxT+1).map(() => (() => [])));

        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);
        let gridSetup = {};
        const drawBoxes = function(t,d,sel_,forceNew) {
            const newCells = forceNew || (t != currTime) || (d != currDim);
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
                    if (showPts) {
                        if (d > 0) {
                            const ptsstring = (pts.length == 0)
                                            ? "(no points)"
                                            : pts.map(pt => "<" + ixPascal(d,pt).join(",") + ">").join(", ");
                            ptsdisp.text(ptsstring).style("font-style", (pts.length == 0) ? "italic" : "normal");
                        } else {
                            ptsdisp.text((pts.length == 0) ? "(no point)" : "(single point)")
                              .style("font-style", "italic");
                        }
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
                    if (showPts) {
                        ptsdisp.text("Hover to view points")
                            .style("font-style","italic");
                    }
                }
            }
            currTime = t;
            currDim = d;
            currSel = sel;
        }
        gridSetup = setupGrid(grid,"",width,height
            ,window_size.width-margin.left-margin.right
            ,window_size.height-margin.top-margin.bottom
            , { onmove: ((x,y) => drawBoxes(currTime,currDim,{x,y},false))
              , onleave: (() => drawBoxes(currTime,currDim,undefined,false))
              }
            );
        if (showPts) {
            gridSetup.grid.on("click", function(e) {
                e.preventDefault();
                locked = !locked;
                if (locked) {
                    gridSetup.underlighter(0.05);
                } else {
                    gridSetup.underlighter(0);
                }
            });
        }
        if (maxDim) {
            const dimselbox = dimslidersvg.append("g")
                            .attr("transform","translate(15,0)");
            const dimslider = d3.sliderBottom()
                .min(0)
                .max(maxDim)
                .step(1)
                .width(slider_size.width-30)
                .ticks(maxDim+1)
                .tickFormat(v => (v+2)+"")
                .displayFormat(v => "d="+(v+2))
                .on("onchange", d => drawBoxes(currTime,d,currSel,false));
            dimselbox.call(dimslider);
            dimslider.value(3)
        }

        const controller =
            setupTimer(slidersvg
                      ,maxT+1
                      ,slider_size.width
                      , i => drawBoxes(i,currDim,currSel,false)
                      );

        return function(newsnaps) {
            return function () {
                snapshots = newsnaps;
                locked = false;
                controller.value(0);
                drawBoxes(0,currDim,undefined,true);
            }
        }
    }
}

// aliveCells : [Thunk [{ x: Int, y: Int, zs: [Int] }]]
exports._setupGol3D = function(sel,{height,width,maxT}) {
    return function () {
        const window_size = { height: 200, width: 200 }
        const margin = { top: 10, bottom: 10, left: 10, right: 10 }
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width*(maxT*2+1), window_size.height])
            .attr("width","100%")
            .style("margin","auto")
            .style("display","block");
        const slidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .style("max-width","15em")
                .style("margin","1em auto")
                .style("display","block")
                .style("overflow","visible");
        const symmetries = d3.range(0,maxT+1).map(z => (z > 0) ? [-z,z] : [z]);
        const cell_size = { height: (window_size.height-margin.top-margin.bottom) / height
                          , width: (window_size.width-margin.left-margin.right) / width
                          }

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
                if (inRange(-maxT,maxT+1,z+dz)) {
                    const hltr = fullGrid[z+dz+maxT].highlighter
                    const baseNeighbs = (dz == 0)
                                      ? neighbs2d.slice(1)
                                      : neighbs2d;
                    hltr(
                        baseNeighbs
                            .map(dxy => ({x:x+dxy.x,y:y+dxy.y,val:1}))
                            .filter(xy => (inRange(0,width,xy.x) && inRange(0,height,xy.y)))
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
                const res = setupGrid(grid,"z = " + z,width,height
                        ,window_size.width-margin.left-margin.right
                        ,window_size.height-margin.top-margin.bottom
                        , { onmove: function(x,y) {
                              highlightNeighbs(x,y,z,i);
                            }
                          , onleave: clearAllHighlights
                          }
                        );
                res.grid
                  .attr("transform",`translate(${(maxT+z)*window_size.width},0)`);
                fullGrid[z+maxT] = {drawer: res.drawer, highlighter: res.highlighter, underlighter: res.underlighter};
                return {drawer: res.drawer, highlighter: res.highlighter, underlighter: res.underlighter};
            });
        });
        let splitCache = [];
        let refresh = [];
        let snapshots = d3.range(maxT+1).map(() => (() => []));
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
            holeyForEach(splitCells, function (z, ps) {
                grids[z].forEach(function ({drawer}) {
                    drawer(ps);
                    refresh.push(drawer);
                });
            });
        }
        const controller =
            setupTimer(slidersvg
                      ,maxT+1
                      ,slider_size.width
                      ,drawBoxes
                      );

        return function(newsnaps) {
            return function () {
                snapshots = newsnaps;
                splitCache = [];
                controller.value(0);
                drawBoxes(0);
            }
        }
    }
}

// aliveCells : [Thunk [{ x: Int, y: Int, zws: [Int] }]]
exports._setupGol4D = function(sel,{height,width,maxT}) {
    const window_size = { height: 200, width: 200 }
    const margin = { top: 10, bottom: 10, left: 10, right: 10 }
    const maxPascal = binom(2+maxT,maxT);
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
    const cell_size = { height: (window_size.height-margin.top-margin.bottom) / height
                      , width: (window_size.width-margin.left-margin.right) / width
                      }
    return function () {
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
            .append("svg")
            .attr("viewBox", [0,0,window_size.width*(maxT*2+1), window_size.height*(maxT*2+1)])
            .attr("width","100%")
            .style("margin","auto")
            .style("display","block");
        const slidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .style("max-width","15em")
                .style("margin","1em auto")
                .style("display","block")
                .style("overflow","visible");

        const grid = svg.append("g")
                .attr("transform",`translate(${margin.left},${margin.top})`);

        let fullGrid = d3.range(2*maxT+1).map(c => []);
        let clearHighlights = [];
        let grids = [];
        const clearAllHighlights = function() {
            clearHighlights.forEach(highlighter => highlighter([]));
            clearHighlights = [];
        }
        const highlightNeighbs = function(x,y,z,w,i) {
            clearAllHighlights();
            neighbs2d.forEach(function (dzw) {
                if (inRange(-maxT,maxT+1,z+dzw.x) && inRange(-maxT,maxT+1,w+dzw.y)) {
                    const hltr = fullGrid[z+dzw.x+maxT][w+dzw.y+maxT].highlighter
                    // hltr([{x,y,val:1}])
                    const baseNeighbs = (dzw.x == 0 && dzw.y == 0)
                                      ? neighbs2d.slice(1)
                                      : neighbs2d;
                    hltr(
                        baseNeighbs
                            .map(dxy => ({x:x+dxy.x,y:y+dxy.y,val:1}))
                            .filter(xy => (inRange(0,width,xy.x) && inRange(0,height,xy.y)))
                    );
                    clearHighlights.push(() => hltr([]));
                }
            });
            grids[i].forEach(function (g) {
                const intensity = (Math.abs(z) == Math.abs(g.point.z) && Math.abs(w) == Math.abs(g.point.w))
                                ? 0.5
                                : 0.25;
                g.underlighter(intensity);
                clearHighlights.push(() => g.underlighter(0));
            });
        }
        grids = symmetries.map(function(pos,i) {
            return pos.map(function({z,w}) {
                const res = setupGrid(grid,z+","+w,width,height
                        ,window_size.width-margin.left-margin.right
                        ,window_size.height-margin.top-margin.bottom
                        , { onmove: function(x,y) {
                              highlightNeighbs(x,y,z,w,i);
                            }
                          , onleave: clearAllHighlights
                          }
                        );
                res.grid
                  .attr("transform",`translate(${(maxT+z)*window_size.width},${(maxT+w)*window_size.height})`);
                fullGrid[z+maxT][w+maxT] = {drawer: res.drawer, highlighter: res.highlighter, underlighter: res.underlighter};
                return {drawer: res.drawer, highlighter: res.highlighter, underlighter: res.underlighter, point: {z,w}};
            });
        });

        let snapshots = d3.range(maxT+1).map(() => (() => []));
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
            holeyForEach(splitCells, function(zw, ps) {
                grids[zw].forEach(function ({drawer}) {
                    drawer(ps);
                    refresh.push(drawer);
                });
            });
        }
        const controller =
            setupTimer(slidersvg
                      ,maxT+1
                      ,slider_size.width
                      ,drawBoxes
                      );

        return function(newsnaps) {
            return function () {
                snapshots = newsnaps;
                splitCache = [];
                controller.value(0);
                drawBoxes(0);
            }
        }
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
    holeyForEach(xs, function(x, ys) {
        holeyForEach(ys, function(y, val) {
            out.push({x,y,val});
        });
    });
    return out;
}

const neighbsArray = function(dim,maxZW,mkPt,reversed) {
    const maxPascal = binom(dim+maxZW,maxZW);
    const neighbDeltas = d3.cross(...d3.range(dim).map(() => [0,-1,1])).slice(1);
    let res = d3.range(0,maxPascal).map( () => ({ neighbBox: [], neighbHighlight: [] }) );
    d3.range(0,maxPascal).forEach(function(i) {
        const pos = ixPascal(dim, i);
        neighbDeltas.forEach(function(dp) {
            const npos = d3.zip(pos,dp).map(([p,dp]) => Math.abs(p+dp));
            npos.sort();
            const j = pascalIx(npos);
            const newPt = mkPt(dp);
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

// ptPos :: Array Int -> { x: Int, y: Int }         -- [0..maxZW]
exports._drawGolSyms = function(sel, maxZ, {dim, gridSize, ptPos}, reversed) {
    const window_size = { height: 200, width: 200 }
    const margin = { top: 10, bottom: 10, left: 10, right: 10 };

    const maxPascal = binom(dim+maxZ,maxZ);

    const neighbs = neighbsArray
            ( dim
            , maxZ
            , (function (x) {
                    const p = ptPos(x.map((y) => y+1));
                    return { x: p.x, y: gridSize.height-1-p.y, val: 1 };
              })
            , reversed
            );
    const topBorder = ptPos(ixPascal(dim,maxPascal-1));
    // console.log(topBorder);

    return function () {
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
                .append("svg")
                // here
                .attr("viewBox", [0,0,window_size.width*(topBorder.x+1), window_size.height*(topBorder.y+1)])
              // .attr("transform",`translate(${window_size.width*z0+margin.left},${window_size.height*(maxZ-w0)+margin.top})`);
                .style("max-width","30em")
                .style("margin","auto")
                .style("display","block")
                .style("overflow","visible");

        const allBoxes = svg.append("g");
        let boxes = [];
        let clearHighlights = [];
        const clearAllHighlights = function () {
            clearHighlights.forEach(clr => clr());
            clearHighlights = [];
        }
        const highlightNeighbs = function (i) {
            clearAllHighlights();
            holeyForEach(neighbs[i].neighbBox, function(j,ps) {
                boxes[j].drawer(ps);
                boxes[j].settext(ps.length + "");
                boxes[j].underlighter(ps.length/8);
                clearHighlights.push(() => boxes[j].drawer([]));
                clearHighlights.push(() => boxes[j].settext(""));
                clearHighlights.push(() => boxes[j].underlighter(0));
            });
            holeyForEach(neighbs[i].neighbHighlight, function(j, ps) {
                boxes[j].highlighter(ps);
                clearHighlights.push(() => boxes[j].highlighter([]));
            });
            if (!(i in neighbs[i].neighbBox)) {
                boxes[i].settext("0");
                clearHighlights.push(() => boxes[i].settext(""));
            }
        }
        boxes = d3.range(0,maxPascal).map(function (i) {
            const zw = ixPascal(dim,i);
            const pos = ptPos(zw);
            const res =
                // here
                setupGrid( allBoxes
                         , zw.join(", ")
                         , gridSize.width, gridSize.height
                         , window_size.width-margin.left-margin.right
                         , window_size.height-margin.top-margin.bottom
                         , { onmove: function() {
                               highlightNeighbs(i);
                             }
                           , onleave: clearAllHighlights
                           }
                         );
            res.grid
              .attr("transform",`translate(${window_size.width*pos.x+margin.left},${window_size.height*(topBorder.y-pos.y)+margin.top})`);
            return { drawer: res.drawer
                   , highlighter: res.highlighter
                   , settext: res.settext
                   , underlighter: res.underlighter
                   };
        });

        return svg;
    }
}

// getNeighbs :: Int -> IntMap Int
exports._drawGolSyms5D = function(sel, getNeighbs) {
    const window_size = { height: 200, width: 200 }
    const maxZWQ = 5;
    const margin = { top: 0, bottom: 0, left: 0, right: 0, inter: 30 };

    const maxPascal = binom(3+maxZWQ,maxZWQ);
    // how many squares to offset for a given Q
    const windowOffset = i => (i == 0) ? 0 : binom(1+i,i-1);
    // how many pixels to offset for a given Q
    const qOffset = i => window_size.width*windowOffset(i) + margin.inter*i;

    let neighbs = [];
    const revNeighbs = d3.range(maxPascal).map(function (i) {
        const ns = getNeighbs(i);
        holeyForEach(ns, function(j, wt) {
            if (!(j in neighbs)) {
                neighbs[j] = [];
            }
            neighbs[j][i] = wt;
        });
        return ns;
    });

    return function () {
        d3.select(sel).selectAll("p").remove();
        const svg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0,qOffset(maxZWQ+1), window_size.height*(maxZWQ+1)])
                .attr("width","100%")
                .style("margin","auto")
                .style("overflow","visible")
                .style("display","block");

        const setupBox = function(g,txt,boxsize,handlers) {
            const box = g.append("g");
            const underlight = box.append("rect")
                    .attr("width",boxsize)
                    .attr("height",boxsize)
                    .attr("fill","yellow")
                    .attr("opacity",0);
            const outline = box.append("rect")
                    .attr("width",boxsize)
                    .attr("height",boxsize)
                    .attr("fill","none")
                    .attr("stroke","black")
                    .attr("stroke-width",2)
                    .attr("stroke-opacity",0.75);
            const mover = function(e) {
                e.preventDefault();
                if ("onmove" in handlers) {
                  handlers.onmove();
                }
            }
            const leaver = function(e) {
                if ("onleave" in handlers) {
                    handlers.onleave();
                }
            }
            const botlab = box.append("text")
                .attr("fill","#333")
                .style("text-anchor","middle")
                .attr("pointer-events","none")
                .attr("font-size",40)
                .attr("font-family",sansSerif)
                .attr("transform", `translate(${boxsize/2},${boxsize*7/8})`)
                .text(txt);
            const mkCircle = function (col) {
                const circcont = box.append("g");
                const circ = circcont.append("circle")
                    .attr("fill",col)
                    .attr("r",0)
                    .attr("transform", `translate(0,${boxsize/2})`)
                    .attr("opacity",0.75)
                    .style("transition", "r 500ms ease");
                const lab = circcont.append("text")
                    .attr("fill","#333")
                    .style("text-anchor","middle")
                    .attr("pointer-events","none")
                    .attr("font-size",50)
                    .attr("font-family",sansSerif)
                    .attr("font-weight","bold")
                    .attr("transform", `translate(0,${boxsize*(3/10)})`);
                const circfunc = function setCirc (d,l) {
                    circ.attr("r",Math.sqrt(d)*boxsize/4);
                    lab.text(l);
                }
                // d should be between 0 and 1
                return { box, circcont, circfunc }
            }
            const circ1 = mkCircle("red");
            circ1.circcont.attr("transform",`translate(${boxsize/4},0)`);
            const circ2 = mkCircle("blue");
            circ2.circcont.attr("transform",`translate(${boxsize*3/4},0)`);
            const capture = box.append("rect")
                    .attr("width",boxsize)
                    .attr("height",boxsize)
                    .attr("fill","white")
                    .attr("opacity",0);
            capture
                  .on("touchstart", mover)
                  .on("touchend", leaver)
                  .on("mouseenter", mover)
                  .on("mouseleave", leaver);
            const underlighter = function(o) {
               underlight.attr("opacity", o);
            }
            return { box, circ1func: circ1.circfunc, circ2func: circ2.circfunc, underlighter };
        }

        const allboxes = svg.append("g");
        let boxes = [];
        let clearHighlights = [];
        const clearNeighbs = function () {
            clearHighlights.forEach(clr => clr());
            clearHighlights = [];
        }
        const highlightNeighbs = function (i) {
            clearNeighbs();
            const to = neighbs[i];
            const from = revNeighbs[i];
            holeyForEach(to, function(x, weight) {
                if (x in boxes) {
                    boxes[x].circ1func(weight/8, weight+"");
                    boxes[x].underlighter(0.2);
                    clearHighlights.push( () => { boxes[x].circ1func(0,""); boxes[x].underlighter(0); } );
                }
            });
            holeyForEach(from, function(x, weight) {
                if (x in boxes) {
                    boxes[x].circ2func(weight/8, weight+"");
                    boxes[x].underlighter(0.2);
                    clearHighlights.push( () => { boxes[x].circ2func(0,""); boxes[x].underlighter(0); } );
                }
            });
        }
        boxes = d3.range(0,maxPascal).map(function (i) {
            const zwq = ixPascal(3,i);
            const z0 = zwq[0];
            const w0 = zwq[1];
            const q0 = zwq[2];
            const boxfuncs =
                setupBox( allboxes
                        , zwq.join(", ")
                        , window_size.width-margin.left-margin.right
                        , { onmove: (() => highlightNeighbs(i)) , onleave: (() => clearNeighbs()) }
                        );
            boxfuncs.box
                .attr("transform",`translate(${qOffset(q0)+window_size.width*z0+margin.left},${window_size.height*(maxZWQ-w0)+margin.top})`);
            return boxfuncs;
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
    const outline = grid.append("rect")
            .attr("width",width)
            .attr("height",height)
            .attr("fill","none")
            .attr("stroke","black")
            .attr("stroke-width",2)
            .attr("stroke-opacity",0.75);
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
            handlers.onmove(x,y);
        }
    }
    const leaver = function(e) {
        if ("onleave" in handlers) {
            handlers.onleave();
        }
    }
    // capture.style("-webkit-tap-highlight-color", "transparent")
    if ("onmove" in handlers) {
        capture
              .on("touchmove", mover)
              .on("touchstart", mover)
              .on("mousemove", mover)
              .on("mouseenter", mover);
    }
    if ("onleave" in handlers) {
        capture
              .on("touchend", () => handlers.onleave())
              .on("mouseleave", () => handlers.onleave());

    }
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
    return sliderino;
}

// f :: a -> [a]
exports._buildHierarchy = function (x,f) {
    return d3.hierarchy(x,f);
}

const expandRun = r => r.flatMap((d, i) => d3.range(d).map(() => i))

// type Mult = { total :: Int, here :: String }
// type Contrib = { chosen :: Array Int
//                , leftovers :: Array Int
//                , multP :: Lazy Mult
//                , multQ :: Lazy Mult
//                , allSame :: Boolean
//                , parts :: { left :: Int, here :: Int, right :: Int }
//                }
//
// getContrib :: Node -> Lazy Contrib
// vecRun :: Int -> Int -> [Int]
// mkHier :: Int -> Int -> Hierarchy
exports._drawTree = function(sel,vecRun,mkHier,getContrib) {
    const margin = { top: 10, bottom: 10, left: 30, right: 42 };
    const optSizeLim = 85;
    const maxZLim = 5;

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

    const expandParts = function(c) {
        const parts = c.parts;
        return [parts.left, parts.here, parts.right].filter(x => !(x === null)).join("+");
    }

    return function () {
        d3.select(sel).selectAll("p").remove();
        const dimslidersvg = d3.select(sel)
                .append("svg")
                .attr("viewBox", [0,0, slider_size.width, slider_size.height])
                .style("max-width","15em")
                .style("margin","0.25em auto 0 auto")
                .style("display","block")
                .style("overflow","visible");
        const formElems = d3.select(sel)
                        .append("form")
                        .style("max-width","15em")
                        .style("margin","0 auto 0 auto")
                        .style("display","block")
        const selbox = formElems
                        .append("select")
                        .style("width","100%");

        const radioopt = [{ label: "forward", checked: "false" }
                         ,{ label: "reverse", checked: "true" }
                         ];
        let forward = false;
        const dirradio = formElems
                        .append("div")
                        .style("width","100%")
                        .style("text-align","center")
                        .style("font-size","90%")
                        .style("margin-top", "0.25em");
        const raddivs = dirradio.selectAll("div")
                            .data(radioopt)
                            .join("div")
                            .attr("class", "pretty p-default p-round");
        const radinps = raddivs.append("input")
                .attr("type","radio")
                .attr("name","neighbdir")
                .attr("value",d => d.label)
                .attr("checked",d => d.checked ? "" : null);
        raddivs.append("div")
                .attr("class","state")
                .append("label")
                .attr("for",d => d.label)
                .text(d => d.label);

        const svg = d3.select(sel)
                .append("svg")
                .attr("width","100%")
                .style("margin","auto")
                .style("overflow","visible")
                .style("display","block");

        const setupSelect = function(dim) {
            selbox.selectAll("*").remove();
            const maxZ = Math.min(maxBinom(dim,optSizeLim) - 1, maxZLim);
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
            .max(6)
            .step(1)
            .width(slider_size.width-30)
            .ticks(6)
            .tickFormat(v => (v+2)+"")
            .displayFormat(v => "d="+(v+2));
        dimselbox.call(dimslider);
        let currpt = []
        currpt[1] = 1
        currpt[2] = 4
        currpt[3] = 6
        currpt[4] = 22
        currpt[5] = 9
        currpt[6] = 9

        const drawTree = function() {
            svg.selectAll("*")
                .remove();
            const dim = dimslider.value();
            const pt = currpt[dim];
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
                .attr("height",19)
                .attr("x",-20)
                .attr("y",6)
                .style("fill","#fff")
                .style("stroke","#666")
                .style("stroke-width","0.5px");

            const tttext1 = tooltip.append("text")
                .attr("text-anchor", "middle")
                .style("font-size",6.5)
                .attr("font-family",sansSerif)
                .attr("x",0)
                .attr("y",14);
            const tttext2 = tooltip.append("text")
                .attr("text-anchor", "middle")
                .style("font-size",6)
                .style("font-style","italic")
                .attr("font-family",sansSerif)
                .attr("x",0)
                .attr("y",21);

            const dots = node.append("circle")
                    .attr("opacity",0.75);

            node.append("text")
                .attr("text-anchor", "end")
                .attr("dy",-1.5)
                .attr("x",-4)
                .attr("opacity", d => ("children" in d) ? 0.9 : 0.5)
                .text(d => getContrib(d.data)().leftovers.join("-"))
                .style("font-weight", d => (d.parent) ? "normal" : "bold");

            node.append("text")
                .attr("text-anchor", "end")
                .attr("dy",4.5)
                .attr("x",-4)
                .attr("opacity", d => (d.parent) ? 0.9 : 0.5)
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
                        // console.log(forward, typeof forward, c.multQ(), c.multP(), m);
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

            const highlightNodes = function(d) {
                const c = getContrib(d.data)();
                const targChosen = c.chosen;
                dots.filter(d => sameArray(targChosen,getContrib(d.data)().chosen))
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
                tttext1.text(expandChosens(c));
                tttext2.text(expandParts(c));
            }
            const clearNodes = function() {
                dots.attr("r",d => (("children" in d) && d.parent) ? 1.5 : 2)
                    .attr("fill",d => (("children" in d) && d.parent) ? "#333" : "red")
                    .attr("opacity","0.75");
                link.selectAll("path")
                    .attr("stroke-opacity",0.4);

                tooltip.attr("display","none");
            }

            // capture
            node.append("circle")
                .attr("r",8)
                .attr("opacity",0)
                .on("mouseenter", (e,d) => highlightNodes(d))
                .on("touchstart", (e,d) => highlightNodes(d))
                .on("mouseleave", () => clearNodes())
                .on("touchend", () => clearNodes());
            clearNodes();


        }

        radinps.on("change", function (d) {
            forward = (d.srcElement.value === "forward");
            // console.log(d.srcElement.value);
            drawTree();
        });

        dimslider.on("onchange", d => { setupSelect(d); drawTree(); } );
        selbox.on("change", function(d) {
            currpt[dimslider.value()] = d.srcElement.value;
            drawTree();
        });
        dimslider.value(4);
        drawTree();

        return svg;
    }
}


exports.undefined = 0;
exports._assignWindow = function(p,x) { return function () { window[p] = x; } }

exports._setInnerHTML = function(e,x) { return function () { e.innerHTML = x; } }
exports.preventDefault = function(e) { return function () { e.preventDefault(); } }
