/**
 * Toggles a checkbox from true to false to indeterminate
 * @param {object} cb The checkbox to toggle
 * 
 * @description 
 * This toggles the state of the checkbox on a click. This allows for the 
 * checkbox to have three states: true, false, and indeterminate.
 */
function ts(cb) {
  if (cb.readOnly) {
    cb.indeterminate=true;
    cb.readOnly=cb.checked=false;
  } else if (!cb.checked) {
    cb.readOnly=true;
    cb.indeterminate=false;
  }
}

/**
 * DataTable callback
 * @param {object} table The DataTable object
 * 
 * @description
 * The function creates two additional event listeners when a checkbox DataTable 
 * is created. The first listens for the column-reorder event and makes sure 
 * that the checkboxes are appropriately displayed for partially reviewed rows. 
 * The second listens for click on the checkbox inputs and updates the DataTable 
 * object accordingly as well as returning the information to the Shiny session.
 */
function checkboxCallback(table) {
  table.on('column-reorder', function() {
    table.rows().every(function() {
      // Only runs for partially reviewed rows.
      if (this.data()[0].reviewed == null) {
        // Aligns the checkbox with the appropriate state for ts()
        $(':checkbox', this.node())
          .addClass('indeterminate')
          .prop('indeterminate', this.data()[0].updated == null)
          .prop('readOnly', this.data()[0].updated == false)
      }
    })
  });
  table.on('click', 'input[type="checkbox"]', function(){
    var tblId = $(this).closest('.datatables').attr('id');
    var cell = table.cell($(this).closest('td'));
    var review = $(this).is(':indeterminate') ? null : $(this).is(':checked');
    // This updates the DataTable object itself. This makes sure the updates 
    //  are maintained when a checkbox is re-rendered but the table is not 
    //  re-drawn.
    cell.data().updated = review;
    var info = {review: review, ids: cell.data().ids, row_id: cell.data().row_id};
    // Returns the information of which checkbox was clicked to the Shiny session.
    Shiny.setInputValue(tblId + '_review_selection:CS.reviewInfo', info, {priority: 'event'});
  })
  return table;
}

/**
 * The DataTable column rendering for checkboxes
 * @param {Any} data The data for the cell
 * @param {string} type The type call data requested.
 * @param {Any} row The full data source for the row.
 * @param {object} meta An object that contains additional information about 
 * the cell being requested.
 * 
 * @description
 * Determines how the checkbox should be rendered (i.e. checked or not) and sets 
 * the class based on the current review status.
 */
function checkboxRender(data, type, row, meta) {
  var reviewed = data.reviewed;
  var updated = data.updated;
  var disabled = data.disabled;
  var cb_class = ''
  // Note reviewed == null means the row is partially reviewed
  if (reviewed == null) {
    cb_class = updated == null ? '' : 'indeterminate'
  } else {
    cb_class = reviewed ? 'checked' : 'unchecked'
  }
  return `<input type='checkbox' 
    ${disabled ? 'disabled ' : ''}
    class='${cb_class}' 
    // If the checkbox has not be clicked, set value to reviewed status, 
    //  otherwise use clicked/updated status
    ${updated == null ? (reviewed ? 'checked' : '') : (updated ? 'checked' : '')} 
    // If the row is partially reviewed. Handle the checkbox state using ts(). 
    //  Using an onclick event is important because we need this to trigger 
    //  before any of the DataTable callbacks.
    ${reviewed == null ? 'onclick="ts(this)"' : ''}/>`;
}

/**
 * The DataTable Row Callback
 * @param {node} row TR element being inserted into the document.
 * @param {array, object} data Data source for the row.
 * 
 * @description
 * Aligns the checkbox with the appropriate state for ts()
 */
function rowCallback(row, data) {
  // Only runs for partially reviewed rows.
  if (data[0].reviewed == null) {
    $(':checkbox', row)
      .addClass('indeterminate')
      .prop('indeterminate', data[0].updated == null)
      .prop('readOnly', data[0].updated == false)
  }
}

$(document).ready(function() {
  
  // Define custom Shiny input binding for overall review checkbox. 
  //  This is needed to assign an event priority to the checkbox.
  var customCheckbox = new Shiny.InputBinding();
  
  $.extend(customCheckbox, {
    find: function(scope) {
      // Only modifies handling for cs_checkbox class.
      return $(scope).find("input[type='checkbox'].cs_checkbox");
    },
    getValue: function(el) {
      return el.checked;
    },
    setValue: function(el, value) {
      // Used in the tests.
      el.checked = value;
    },
    subscribe: function(el, callback) {
      $(el).on("change.checkboxInputBinding", function() {
        // Sets the priority to event so that it triggers every time it's clicked.
        Shiny.onInputChange($(this).attr('id'), this.checked, {priority: 'event'});
      });
    },
    unsubscribe: function(el) {
      $(el).off(".checkboxInputBinding");
    }
  });
  
  Shiny.inputBindings.register(customCheckbox);
  
  // Define custom Shiny output binding for review progress bar. 
  //  It expects 4 values: completed, unmarking, marking, and total.
  var customProgressBar = new Shiny.OutputBinding();
  
  $.extend(customProgressBar, {
    find: function(scope) {
      return $(scope).find("div.cs-progress-container");
    },
    renderValue: function(el, data) {
      let cmp_pct = (data.completed-data.unmarking)/data.total*100;
      let um_pct = data.unmarking/data.total*100;
      let m_pct = data.marking/data.total*100;
      let true_cmp_pct = data.completed/data.total*100;
      if (data.total == 0) {
        // Sets width based on the number of records in a category.
        $('#' + el.id + " .cs-progress.completed").width("100%")
        $('#' + el.id + " .cs-progress.unmarking").width("0%")
        $('#' + el.id + " .cs-progress.marking").width("0%")
        $('#' + el.id + " .cs-completed").html("100.0%")
      } else {
        // Sets form to completely reviewed if there is not data.
        $('#' + el.id + " .cs-progress.completed").width(cmp_pct.toFixed(2) + "%")
        $('#' + el.id + " .cs-progress.unmarking").width(um_pct.toFixed(2) + "%")
        $('#' + el.id + " .cs-progress.marking").width(m_pct.toFixed(2) + "%")
        $('#' + el.id + " .cs-completed").html(true_cmp_pct.toFixed(1) + "%")
      }
    }
  });
  
  Shiny.outputBindings.register(customProgressBar)

});
