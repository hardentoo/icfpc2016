

window.addEventListener("DOMContentLoaded", function() {
  const textarea        = document.querySelector("textarea");
  const canvas          = document.querySelector("canvas");
  const maxProblems     = document.querySelector("#problems-count");
  const currentProblem  = document.querySelector("#current-problem");
  const parseButton     = document.querySelector("#parse-button");
  const prevButton      = document.querySelector("#prev-button");
  const nextButton      = document.querySelector("#next-button");
  const previews        = document.querySelector("#previews");

  const renderer  = new Renderer(canvas);

  const renderUI = function() {
    renderer.render(window.problemSet.problems[window.currentProblem], window.problemSet, true);

    maxProblems.innerText = window.problemSet.problems.length;
    currentProblem.value = window.currentProblem + 1;
  }

  const renderPreviews = function() { 
    previews.innerHTML = "";

    for(let i = 0; i < window.problemSet.problems.length; i++) {
      let problem = window.problemSet.problems[i];
      let canvas = document.createElement("canvas");
      canvas.classList.add("preview-canvas");
      canvas.width  = "100";
      canvas.height = "100";
      // can
      canvas.setAttribute("data-problem-index", i);
      canvas.addEventListener("click", onPreview);

      previews.appendChild(canvas);

      let previewRenderer = new Renderer(canvas);
      previewRenderer.render(problem, window.problemSet);
    }
  }

  const onParse = function() {
    window.problemSet = Parser.parse(textarea.value);
    window.currentProblem = 0;

    renderUI();
    renderPreviews();
  };

  const onPrev = function() {
    if(window.currentProblem > 0) {
      window.currentProblem--;
      renderUI();
    }
  }

  const onNext = function() {
    if(window.currentProblem < window.problemSet.problems.length-1) {
      window.currentProblem++;
      renderUI();
    }
  }

  const onPreview = function(event) {
    console.log("event.target.getAttribute", event.target.getAttribute("data-problem-index"));
    window.currentProblem = Number.parseInt(event.target.getAttribute("data-problem-index"));
    renderUI();
  }

  const onKeyDown = function(event) {
    console.log("event.key", event.key);
    if(event.key == "ArrowLeft" || event.key == "ArrowUp") { onPrev(); }
    else if(event.key == "ArrowRight" || event.key == "ArrowDown") { onNext(); }
  }

  parseButton.addEventListener("click", onParse);
  prevButton.addEventListener("click", onPrev);
  nextButton.addEventListener("click", onNext);

  window.addEventListener("keydown", onKeyDown);
  onParse();
});

class Polygon {
  constructor(pt) {
    this.points = pt;
    
  }
}

class Problem { 
  constructor(pg, bn) {
    this.polygons = pg;
    this.bones  = bn;
  }
}

class ProblemSet {
  constructor(pbs, cx0, cy0, cx1, cy1) {
    this.problems = pbs;
    this.x0 = cx0;
    this.y0 = cy0;
    this.x1 = cx1;
    this.y1 = cy1;
  }
}

class Renderer {
  constructor(cvs) {
    this.canvas   = cvs;
    this.context  = cvs.getContext("2d");
  }
  
  render(problem, set, renderLabels) {
    let width = this.canvas.width * 0.8;
    let height = this.canvas.height * 0.8;
    let left = this.canvas.height * 0.1;
    let top = this.canvas.height * 0.1;

    let scaleX = 1 / (set.x1 - set.x0);
    let scaleY = 1 / (set.y1 - set.y0);

    this.context.clearRect(0, 0, this.canvas.width, this.canvas.height);
    for(let polygon of problem.polygons) {
      this.context.beginPath();

      let first = true;

      for(let point of polygon.points) {
        if(first) {
          this.context.moveTo(left + ((point.x - set.x0) * width * scaleX), top + (height - ((point.y - set.y0) * height * scaleY)));
          first = false;
        } else {
          this.context.lineTo(left + ((point.x - set.x0) * width * scaleX), top + (height - ((point.y - set.y0) * height * scaleY)));
        }
      }

      this.context.closePath();

      this.context.fillStyle = "rgb(248, 131, 121)";
      this.context.fill();

      this.context.beginPath();
    }

    for(let bone of problem.bones) {
      this.context.moveTo(left + ((bone.p0.x - set.x0) * width * scaleX), top + (height - ((bone.p0.y - set.y0) * height * scaleY)));
      this.context.lineTo(left + ((bone.p1.x - set.x0) * width * scaleX), top + (height - ((bone.p1.y - set.y0) * height * scaleY)));
    }

    this.context.strokeStyle = "rgb(0, 0, 0)";
    this.context.lineWidth = 2;

    this.context.stroke();

    this.context.fillStyle = "rgb(0, 0, 0)";
    this.context.font = "14px sans-serif";

    if(renderLabels) {
      this.context.fillText(set.x0.toString().slice(0, 5), left + 4, top + height + 16, 20);
      this.context.fillText(set.x1.toString().slice(0, 5), left + width, top + height + 16, 20);
      this.context.fillText(set.y0.toString().slice(0, 5), left - 25, top + height, 20);
      this.context.fillText(set.y1.toString().slice(0, 5), left - 25, top + 12, 20);
    }
  }
}

function nestedBest(problems, start, axis, comparator) {
  return problems.reduce(function(b1, problem) {
    let sub = problem.polygons.reduce(function(b2, polygon) {
      let subsub = polygon.points.reduce(function(b3, point) {
        return (comparator(point[axis], b3) ? point[axis] : b3);
      }, start);
      return (comparator(subsub, b2) ? subsub : b2);
    }, start);
    return (comparator(sub, b1) ? sub : b1);
  }, start);
}

class Parser {
  static parse(input) {
    const lines     = input.split("\n").filter(s => s !== "");
    const problems  = [];

    while(lines.length > 0) {
      let polygons = [];

      const numberOfPolygons = lines.shift();

      
      for(let i = 0; i < numberOfPolygons; i++) {
        polygons.push(Parser.parsePolygon(lines));
      }

      let bones = Parser.parseBones(lines);

      problems.push(new Problem(polygons, bones));
    }

    return new ProblemSet(problems,
      nestedBest(problems, 1e+1000000,  "x", (a, b) => a < b),
      nestedBest(problems, 1e+10000,  "y", (a, b) => a < b),
      nestedBest(problems, 1e-10000000, "x", (a, b) => a > b),
      nestedBest(problems, 1e-10000, "y", (a, b) => a > b)
    );
  }

  static parsePolygon(lines) {
    const numberOfPoints = lines.shift();
    let points = [];
    for(let i = 0; i < numberOfPoints; i++) {
      points.push(Parser.parsePoint(lines.shift()));
    }
    return new Polygon(points);
  }

  static parseBones(lines) {
    const numberOfBones = lines.shift();
    let bones = [];
    for(let i = 0; i < numberOfBones; i++) {
      bones.push(Parser.parseBone(lines.shift()));
    }
    return bones;
  }

  static parseBone(input) {
    let components = input.split(" ");
    return {
      p0: Parser.parsePoint(components[0]),
      p1: Parser.parsePoint(components[1])
    };
  }

  static parsePoint(input) {
    let components = input.split(",");

    return { 
      x: Parser.parseCoordinate(components[0]),
      y: Parser.parseCoordinate(components[1])
    };
  }

  static parseCoordinate(input) {
    let components = input.split("/");
    if(components.length == 1) {
      return Number.parseInt(components[0]);
    } else {
      return Number.parseInt(components[0]) / Number.parseInt(components[1]);
    }
  }
}
