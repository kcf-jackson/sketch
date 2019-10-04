p5$new(function(sketch) {
    sketch$setup = function() {
        sketch$createCanvas(800, 300)
    }
    sketch$draw = function() {
        sketch$background(0,0,33)

        sketch$fill(255,200,0)
        dia = sketch$sin(sketch$frameCount*0.025) * 200
        sketch$ellipse(200, 150, dia, dia)

        sketch$fill(135,255,30)
        dia2 = sketch$sin(sketch$frameCount*0.05) * 200
        sketch$ellipse(400, 150, dia2, dia2)

        sketch$fill(30,0,255)
        dia3 = sketch$sin(sketch$frameCount*0.015) * 200
        sketch$ellipse(600, 150, dia3, dia3)
    }

    return(sketch)
}, "p5_canvas")
