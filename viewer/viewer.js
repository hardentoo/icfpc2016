

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
    renderer.render(window.problemSet[window.currentProblem]);

    maxProblems.innerText = window.problemSet.length;
    currentProblem.value = window.currentProblem + 1;


  }

  const renderPreviews = function() { 
    previews.innerHTML = "";

    for(let i = 0; i < window.problemSet.length; i++) {
      let problem = window.problemSet[i];
      let canvas = document.createElement("canvas");
      canvas.classList.add("preview-canvas");
      canvas.width  = "100";
      canvas.height = "100";
      // can
      canvas.setAttribute("data-problem-index", i);
      canvas.addEventListener("click", onPreview);

      previews.appendChild(canvas);

      let previewRenderer = new Renderer(canvas);
      previewRenderer.render(problem);
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
    if(window.currentProblem < window.problemSet.length) {
      window.currentProblem++;
      renderUI();
    }
  }

  const onPreview = function(event) {
    console.log("event.target.getAttribute", event.target.getAttribute("data-problem-index"));
    window.currentProblem = Number.parseInt(event.target.getAttribute("data-problem-index"));
    renderUI();
  }

  parseButton.addEventListener("click", onParse);
  prevButton.addEventListener("click", onPrev);
  nextButton.addEventListener("click", onNext);
  onParse();
});

class Polygon {
  constructor(pt, bn) {
    this.points = pt;
    this.bones  = bn;
  }
}

class Problem { 
  constructor(pg) {
    this.polygons = pg;
  }
}

class Renderer {
  constructor(cvs) {
    this.canvas   = cvs;
    this.context  = cvs.getContext("2d");
  }
  
  render(problem) {
    let width = this.canvas.width;
    let height = this.canvas.height;

    this.context.clearRect(0, 0, width, height);
    for(let polygon of problem.polygons) {
      this.context.beginPath();

      let first = true;

      for(let point of polygon.points) {
        if(first) {
          this.context.moveTo(point.x * width, height - (point.y * height));
          first = false;
        } else {
          this.context.lineTo(point.x * width, height - (point.y * height));
        }
      }

      this.context.closePath();

      this.context.fillStyle = "rgb(248, 131, 121)";
      this.context.fill();

      this.context.beginPath();


      for(let bone of polygon.bones) {
        this.context.moveTo(bone.p0.x * width, height - (bone.p0.y * height));
        this.context.lineTo(bone.p1.x * width, height - (bone.p1.y * height));
      }

      this.context.strokeStyle = "rgb(0, 00, 0)";
      this.context.lineWidth = 2;

      this.context.stroke();
    }
  }
}

class Parser {
  static parse(input) {
    const lines     = input.split("\n");
    const problems  = [];

    while(lines.length > 0) {
      let polygons = [];
      

      const numberOfPolygons = lines.shift();
      
      for(let i = 0; i < numberOfPolygons; i++) {
        polygons.push(Parser.parsePolygon(lines));
      }

      problems.push(new Problem(polygons));
    }

    return problems;
  }

  static parsePolygon(lines) {
    const numberOfPoints = lines.shift();
    let points = [];
    for(let i = 0; i < numberOfPoints; i++) {
      points.push(Parser.parsePoint(lines.shift()));
    }

    const numberOfBones = lines.shift();
    let bones = [];
    for(let i = 0; i < numberOfBones; i++) {
      bones.push(Parser.parseBone(lines.shift()));
    }

    return new Polygon(points, bones);
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