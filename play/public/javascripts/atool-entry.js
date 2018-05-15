(function(window){

    function callBind() {
        var entryPoint = document.getElementById('atool-entry');
        var elem = document.getElementById('annotation-tool');
        var apiUrl = entryPoint.getAttribute('data-entry-url');
        var csrfToken = entryPoint.getAttribute("data-csrf-token");
        var isAdmin = entryPoint.getAttribute("data-is-admin") === "true";
        var uid = entryPoint.getAttribute("data-uid");
        window.AnnotationTool.bind(apiUrl, elem, csrfToken, isAdmin, uid);
    }

    function anonEditor() {
        var entryPoint = document.getElementById('analysis-editor');
        var apiUrl = entryPoint.getAttribute('data-entry-url');
        var csrfToken = entryPoint.getAttribute("data-csrf-token");
        var uid = entryPoint.getAttribute("data-uid");
        window.AnnotationTool.anonEditor(apiUrl, entryPoint, csrfToken, uid);
    }

    document.addEventListener('DOMContentLoaded', function(ev) {
        var entryPoint = document.getElementById('atool-entry');
        if (entryPoint !== null) {
            callBind();
        }

        entryPoint = document.getElementById('analysis-editor');
        if (entryPoint !== null) {
            anonEditor();
        }
    });
})(window);