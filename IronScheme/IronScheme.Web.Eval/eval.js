
var edargs = 
{ 
  lineNumbers: true, 
  matchBrackets: true,
  tabMode: 'shift',
  indentUnit: 2 
};

var editor = CodeMirror.fromTextArea(document.getElementById('expr'), edargs);

$('#eform').submit(function(event) {
  editor.save();
  event.preventDefault();

  var status = $('#status');
  var error = $('#error pre');
  var output = $('#output pre');
  var result = $('#result pre');

  status.text('Please wait...');
  error.text('');
  output.text('');
  result.text('');

  var x = $.post('index.ss', { expr: $("#expr").val() },
      function(data) {
        status.text("Completed");
        error.text(data.error);
        output.text(data.output);
        result.text(data.result);
      }, 'json');
  x.error(function() { status.text("Server error"); });
});

$('#load').click(function(event) {
  event.preventDefault();
  $.getJSON('snippet.ss?id=' + 1, function(data) {
    editor.setValue(data.content);
  });
});

$('#save').click(function(event) {
  event.preventDefault();
  editor.save();
  
  var status = $('#status');
  
  var x = $.post('snippet.ss', { expr: $('#expr').val() }, 
            function(data) {
              editor.setValue(data.content);
            }, 'json');
  x.error(function() { status.text("Server error"); });            
});

