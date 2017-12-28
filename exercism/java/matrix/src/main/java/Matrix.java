class Matrix {

  int[][] _entries;
  int _nrows;
  int _ncols;

  Matrix(String matrixAsString) {
    String[] lines = matrixAsString.split("[\\r\\n]+");
    _nrows = lines.length;
    _ncols = lines[0].split("\\s+").length;
    _entries = new int[_nrows][_ncols];
    for (int i = 0; i < _nrows; i++) {
      String[] row = lines[i].split("\\s+");
      for (int j = 0; j < _ncols; j++) {
        _entries[i][j] = Integer.parseInt(row[j]);
      }
    }
  }

  int[] getRow(int rowNumber) {
    return _entries[rowNumber];
  }

  int[] getColumn(int columnNumber) {
    int[] c = new int[_entries.length];
    for (int i = 0; i < _entries.length; i++) {
      c[i] = _entries[i][columnNumber];
    }
    return c;
  }

  int getRowsCount() {
    return _nrows;
  }

  int getColumnsCount() {
    return _ncols;
  }
}
