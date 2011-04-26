// column formats for gtable. Based on types
// text is red or black depending
gtableNumeric = function(val) { 
    return '<span style="color:red">' + val + '</span>';
}
    
gtableInteger = function(val) { 
    return '<span style="color:red">' + val + '</span>';
};

gtableLogical = function(val) { 
    return '<span style="color:black">' + val + '</span>';
};

gtableIcon = function(val, metaData, record, rowIndex, columnIndex, store) { 
    metaData.css = val;
    metaData.attr = 'style="background-repeat:no-repeat"'
    return '';
}

gtableDate = function(val) {
    return '<span style="color:red">' + val + '</span>';
}
