function docall() {
  $.post('/', { expr: $("#expr").val() },
      function (data) {
        $("#result").html(data);
      });
  return false;
}

var editor = CodeMirror.fromTextArea(document.getElementById("expr"),
                                      { lineNumbers: true, 
                                        matchBrackets: true,
                                        tabMode: "shift",
                                        indentUnit: 2 });

$("#eform").submit(function (event) {
  editor.save();
  $("#result").text('Please wait...');
  event.preventDefault();
  return docall();
});


