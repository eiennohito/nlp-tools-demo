"use strict";

(function () {
    var backendUrl = null;
    function doRender() {
        if (backendUrl == null) {
            console.log("backendUrl == null");
            return
        }

        var content =  $("#data").val();
        if (content.length > 0) {
            window.GraphRenderer.render(backendUrl, content).then(function(g) {
                console.log(g);
                var svg = d3.select("#graph");
                svg.select('g').remove();
                var inner = svg.append("g");

                var render = new dagreD3.render();
                render(inner, g);

                inner.attr("transform", "translate(0, 30)");
                svg.attr("height", g.graph().height + 60);
                svg.attr("width", g.graph().width);

                inner.selectAll('g.node')
                    .each(function (v) {
                        $(this).tipsy({
                            opacity: 0.8, html: true, gravity: 's',
                            title: function() { return g.node(v).tooltip; }
                        })
                    });

                var $top = $("#top-dom");
                $top.removeClass('reporting');

                var topDom = GraphRenderer.objReprFor(1);
                var detailed = false;
                var btn = $(topDom).find("button.report-btn");
                btn.on('click', function() {
                    var reportUri = "@routes.Jumanpp.report()";
                    if (detailed) {
                        $.ajax(reportUri, {
                            method: "POST",
                            data: JSON.stringify({
                                id: GraphRenderer.latticeId(),
                                ids: GraphRenderer.markedIds()
                            }),
                            contentType: "application/json",
                            processData: false
                        }).then(function () {
                            $(".report-line").remove();
                        });
                    } else {
                        detailed = true;
                        $("#top-dom").addClass('reporting');
                        btn.text("送信");
                        $.ajax(reportUri, {
                            method: "POST",
                            data: JSON.stringify({id: GraphRenderer.latticeId(), ids: []}),
                            contentType: "application/json",
                            processData: false
                        })
                    }
                });
                $top.empty().append(topDom);
            });
        }
    }

    $("#send").on('click', function (e) {
        doRender();
        return false;
    });

    $(document).ready(function () {
        var obj = document.getElementById('jumanpp-script');
        backendUrl = obj.getAttribute('data-backend-url');
        doRender();
    });
}());
