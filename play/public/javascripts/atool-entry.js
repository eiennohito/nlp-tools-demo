(function(window){
    document.addEventListener('DOMContentLoaded', function(ev) {
        var entryPoint = document.getElementById('atool-entry');
        var elem = document.getElementById('annotation-tool');
        var apiUrl = entryPoint.getAttribute('data-entry-url');
        var csrfToken = entryPoint.getAttribute("data-csrf-token");
        var isAdmin = entryPoint.getAttribute("data-is-admin") === "true";
        window.AnnotationTool.bind(apiUrl, elem, csrfToken, isAdmin);
    });
})(window);