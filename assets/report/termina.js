// Sorting of the tables and folding of the groups of rows in the reports of termina
(function () {
  'use strict';

  // The key of a cell: a number, a string in lower case, or null when empty
  function key(cell) {
    if (!cell) { return null; }
    var text = cell.hasAttribute('data-sort') ? cell.getAttribute('data-sort') : cell.textContent.trim();
    if (text === '' || text === '-') { return null; }
    return /^-?\d+(\.\d+)?$/.test(text) ? parseFloat(text) : text.toLowerCase();
  }

  // Empty cells go last in both directions
  function compare(column, direction) {
    return function (a, b) {
      var x = key(a.cells[column]), y = key(b.cells[column]);
      if (x === null || y === null) { return (x === null) - (y === null); }
      return (x < y ? -1 : x > y ? 1 : 0) * direction;
    };
  }

  function head(body) { return body.querySelector('tr.head'); }

  function sortTable(table, column, direction) {
    var bodies = Array.prototype.slice.call(table.tBodies);
    bodies.forEach(function (body) {
      var rows = Array.prototype.filter.call(body.rows, function (r) { return !r.classList.contains('head'); });
      rows.sort(compare(column, direction)).forEach(function (r) { body.appendChild(r); });
    });
    if (bodies.length > 1 && bodies.every(head)) {
      var byHead = compare(column, direction);
      bodies.sort(function (a, b) { return byHead(head(a), head(b)); })
        .forEach(function (body) { table.appendChild(body); });
    }
  }

  document.querySelectorAll('table.sortable').forEach(function (table) {
    var cells = table.tHead.rows[0].cells;
    Array.prototype.forEach.call(cells, function (th, column) {
      th.addEventListener('click', function () {
        var numeric = th.classList.contains('n');
        var direction = th.classList.contains('desc') ? 1 : th.classList.contains('asc') ? -1 : (numeric ? -1 : 1);
        Array.prototype.forEach.call(cells, function (c) { c.classList.remove('asc', 'desc'); });
        th.classList.add(direction > 0 ? 'asc' : 'desc');
        sortTable(table, column, direction);
      });
    });
  });

  var groups = document.querySelectorAll('tbody.group');
  groups.forEach(function (body) {
    body.classList.add('closed');
    head(body).addEventListener('click', function () { body.classList.toggle('closed'); });
  });
  document.querySelectorAll('[data-fold]').forEach(function (button) {
    button.hidden = false;
    button.addEventListener('click', function () {
      var close = button.getAttribute('data-fold') === 'close';
      groups.forEach(function (body) { body.classList.toggle('closed', close); });
    });
  });
})();
