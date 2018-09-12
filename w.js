// Welsh-Powell-Algorithm
function coloring(es) {
  var ns = neighbors(es),cs = {}, c = 0 
  ns.sort(([_a1,e1],[_a2,e2])=>e2.length-e1.length)
  ns.forEach(([a1,e1],i)=>(a1 in cs || (cs[a1] = ++c),
    ns.slice(i+1).forEach(([a,e])=>
      a in cs||e1.includes(a)||e.some(v=>cs[v]==c)||(cs[a]=c))))
  return [cs,es]
}
// 隣を求める
function neighbors(es) {
  var ns = {}
  es.forEach(([a,b])=>{
    if (!(a in ns)) ns[a]=[]
    if (!(b in ns)) ns[b]=[]
    ns[a].push(b)
    ns[b].push(a)
  })
  return Object.keys(ns).map(i=>[i,ns[i]])
}
// 画像生成
function dot([cs,es]) {
  return [].concat(
    ["graph G{\n"],
    Object.keys(cs).map(c=>"\t"+c+" [label=\""+c+" r"+cs[c]+"\"]\n"),
    es.map(([a,b])=>"\t" + a + " -- " + b + "\n"),
    ["}"]
  ).join("")
}
// 実行
function run(es) {
  require('fs').writeFileSync("w.dot", dot(coloring(es)))
  console.log(require('child_process').execSync("dot -Tpng -ow.png w.dot").toString())
}
/*
run([
  ["a","f"],
  ["f","e"],["f","m"],["f","j"],["f","z"],["f","b"],["f","c"],
  ["e","z"],["e","h"],["e","b"],["e","m"],
  ["j","k"],["j","d"],["j","h"],["j","g"],["j","z"],["j","b"],["j","c"],
  ["k","b"coloring],["k","d"],["k","g"],["k","z"],["k","b"],["k","c"],
  ["b","m"],["b","d"],["b","c"],
  ["m","c"],["m","d"],
  ["h","g"]
])
run([['x','1'],['y','2'],['4','1'],['5','2'],['6','4'],['6','5'],['4','5'],['8','1'],['9','8'],
     ['18','1'],['11','1'],['12','11'],['12','1'],['14','1'],['15','14'],['15','1']])
*/


// ベーシックブロック内を解析して入出力を求める
function liveness(g) {
  var g2=JSON.stringify(g)
  do {
    var g1=g2
    Object.entries(g).reverse().forEach(([k,v])=>{
      v.out=v.out?v.out:new Set()
      v.inp=v.inp?v.inp:new Set()
      v.br.forEach(b=>"inp" in g[b] && g[b].inp.forEach(i=>{v.out.add(i);v.inp.add(i)}))
      if(v.br.length==0)
        v.block[v.block.length-1][1].forEach(i=>{v.out.add(i);v.inp.add(i)})
      v.block.slice().reverse().forEach(([out,inp])=>{
        inp.forEach(i=>{v.inp.add(i)})
        out.forEach(o=>{v.inp.delete(o)})
      })
    })
  } while(g1!=(g2=JSON.stringify(g)))
}
// グラフから辺を求める
function gen_edges(g) {
  var es = []
  function addEs([a,b]) {
    if (a==b) return
    if (es.some(([x,y])=>x==a&&y==b||x==b&&y==a)) return
    es.push([a,b])
  }
  function add(arr) {
    arr.forEach((x,i)=>arr.slice(i+1).forEach(y=>addEs([x,y])))
  }
  Object.entries(g).forEach(([k,v])=>{
    var live = v.out
    add([...live])
    v.block.slice().reverse().forEach(([out,inp])=>{
      add([...out,...inp])
      out.forEach(o=>live.delete(o))
      inp.forEach(i=>live.forEach(l=>addEs([i,l])))
      live = new Set([...live,...inp])
    })
  })
  return es
}

var g = {
  0 : {block:[
    [['1'],[]],
    [['2'],[]],
    [[],['x','1']],
    [[],['y','2']],
    [[],[]]
  ],br:['3']},
  3 : {block:[
    [['4'],['1']],
    [['5'],['2']],
    [['6'],['4','5']],
    [[],['6']]
  ],br:['7','17']},
  7 : {block:[
    [['8'],['1']],
    [['9'],['8']],
    [[],['9']]
  ],br:['10','13']},
  10 : {block:[
    [['11'],['1']],
    [['12'],['11']],
    [[],['12','1']],
    [[],[]]
  ],br:['16']},
  13 : {block:[
    [['14'],['1']],
    [['15'],['14']],
    [[],['15','1']],
    [[],[]]
  ],br:['16']},
  16 : {block:[
    [[],[]]
  ],br:['3']},
  17 : {block:[
    [['18'],['1']],
    [[],['18']]
  ],br:[]}
}
liveness(g)
var es = gen_edges(g)
run(es)
