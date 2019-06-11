package test;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;

//scala.collection.immutable.List
public class JavaTableList {

    private static final String[] BLINE = { "-", "\u2501" };
    private static final String[] CROSSING = { "+", "\u2548" };
    private static final String[] VERTICAL_TSEP = { "|", "\u2502" };
    private static final String[] VERTICAL_BSEP = { "|", "\u2503" };
    private static final String TLINE = "\u2500";
    private static final String CORNER_TL = "\u250c";
    private static final String CORNER_TR = "\u2510";
    private static final String CORNER_BL = "\u2517";
    private static final String CORNER_BR = "\u251b";
    private static final String CROSSING_L = "\u2522";
    private static final String CROSSING_R = "\u252a";
    private static final String CROSSING_T = "\u252c";
    private static final String CROSSING_B = "\u253b";

    private String[] descriptions;
    private ArrayList<String[]> table;
    private int[] tableSizes;
    private int rows;
    private int findex;
    private String filter;
    private boolean ucode;
    private Comparator<String[]> comparator;
    private int spacing;
    private EnumAlignment aligns[];

    public JavaTableList(List<String> descriptions) {
        this(descriptions.size(), descriptions);
    }

    public JavaTableList(int columns, List<String> descriptionsList) {
        String[] descriptions = new String[descriptionsList.size()];
        descriptionsList.toArray(descriptions);
        if (descriptions.length!= columns) {
            throw new IllegalArgumentException();
        }
        this.filter = null;
        this.rows = columns;
        this.descriptions = descriptions;
        this.table = new ArrayList<>();
        this.tableSizes = new int[columns];
        this.updateSizes(this.descriptions);
        this.ucode = false;
        this.spacing = 1;
        this.aligns = new EnumAlignment[columns];
        this.comparator = null;
        for (int i = 0; i < aligns.length; i++) {
            aligns[i] = EnumAlignment.CENTER;
        }
    }

    private void updateSizes(String[] elements) {
        for (int i = 0; i < tableSizes.length; i++) {
            if (elements[i] != null) {
                int j = tableSizes[i];
                j = Math.max(j, elements[i].length());
                tableSizes[i] = j;
            }
        }
    }

    public JavaTableList compareWith(Comparator<String[]> c) {
        this.comparator = c;
        return this;
    }

    public JavaTableList sortBy(int column) {
        return this.compareWith((o1, o2) -> o1[column].compareTo(o2[column]));
    }

    public JavaTableList align(int column, EnumAlignment align) {
        aligns[column] = align;
        return this;
    }

    public JavaTableList withSpacing(int spacing) {
        this.spacing = spacing;
        return this;
    }

    /**
     * Adds a row to the table with the specified elements.
     */

    public JavaTableList addRow(List<String> elementsList) {
        String[] elements = new String[elementsList.size()];
        elementsList.toArray(elements);
        if (elements.length != rows) {
            throw new IllegalArgumentException();
        }
        table.add(elements);
        updateSizes(elements);
        return this;
    }

    public JavaTableList filterBy(int par0, String pattern) {
        this.findex = par0;
        this.filter = pattern;
        return this;
    }

    public JavaTableList withUnicode(boolean ucodeEnabled) {
        this.ucode = ucodeEnabled;
        return this;
    }

    public String toString(int numOfTabs) {
        StringBuilder line = new StringBuilder();
        boolean isNewLine = true;

        StringBuilder tabsBuilder = new StringBuilder();
        for (int i = 0; i < numOfTabs; ++i) tabsBuilder.append("\t");

        if (ucode) {
            for (int i = 0; i < rows; i++) {
                if (!isNewLine) {
                    line.append(CROSSING_T);
                } else {
                    isNewLine = false;
                    line.append(tabsBuilder);
                    line.append(CORNER_TL);
                }
                for (int j = 0; j < tableSizes[i] + 2 * spacing; j++) {
                    line.append(TLINE);
                }
            }
            line.append(CORNER_TR);
            line.append(System.lineSeparator());

            isNewLine = true;
        }

        // print header
        for (int i = 0; i < rows; i++) {
            if (!isNewLine) {
                line.append(gc(VERTICAL_TSEP));
            } else {
                isNewLine = false;
                line.append(tabsBuilder);
                if (ucode) {
                    line.append(gc(VERTICAL_TSEP));
                }
            }
            StringBuilder part = new StringBuilder(descriptions[i]);
            while (part.length() < tableSizes[i] + spacing) {
                part.append(" ");
            }
            for (int j = 0; j < spacing; j++) {
                line.append(" ");
            }
            line.append(part);
        }
        if (ucode) {
            line.append(gc(VERTICAL_TSEP));
        }

        line.append(System.lineSeparator());
        isNewLine = true;

        for (int i = 0; i < rows; i++) {
            if (!isNewLine) {
                line.append(gc(CROSSING));
            } else {
                isNewLine = false;
                line.append(tabsBuilder);
                if (ucode) {
                    line.append(CROSSING_L);
                }
            }
            for (int j = 0; j < tableSizes[i] + 2 * spacing; j++) {
                line.append(gc(BLINE));
            }
        }
        if (ucode) {
            line.append(CROSSING_R);
        }

        line.append(System.lineSeparator());
        isNewLine = true;

        ArrayList<String[]> localTable = table;

        if (filter != null) {
            Pattern p = Pattern.compile(filter);
            localTable.removeIf(arr -> {
                String s = arr[findex];
                return !p.matcher(s).matches();
            });
        }

        if (localTable.isEmpty()) {
            String[] sa = new String[rows];
            localTable.add(sa);
        }

        localTable.forEach(arr -> {
            for (int i = 0; i < arr.length; i++) {
                if (arr[i] == null) {
                    arr[i] = "";
                }
            }
        });

        if (comparator != null) {
            localTable.sort(comparator);
        }

        for (String[] strings : localTable) {
            for (int i = 0; i < rows; i++) {
                if (!isNewLine) {
                    line.append(gc(VERTICAL_BSEP));
                } else {
                    isNewLine = false;
                    line.append(tabsBuilder);
                    if (ucode) {
                        line.append(gc(VERTICAL_BSEP));
                    }
                }
                StringBuilder part = new StringBuilder();
                for (int j = 0; j < spacing; j++) {
                    part.append(" ");
                }
                if (strings[i] != null) {
                    switch (aligns[i]) {
                        case LEFT:
                            part.append(strings[i]);
                            break;
                        case RIGHT:
                            for (int j = 0; j < tableSizes[i] - strings[i].length(); j++) {
                                part.append(" ");
                            }
                            part.append(strings[i]);
                            break;
                        case CENTER:
                            for (int j = 0; j < (tableSizes[i] - strings[i].length()) / 2; j++) {
                                part.append(" ");
                            }
                            part.append(strings[i]);
                            break;
                    }
                }
                while (part.length() < tableSizes[i] + spacing) {
                    part.append(" ");
                }
                for (int j = 0; j < spacing; j++) {
                    part.append(" ");
                }
                line.append(part);
            }
            if (ucode) {
                line.append(gc(VERTICAL_BSEP));
            }

            line.append(System.lineSeparator());
            isNewLine = true;
        }

        if (ucode) {
            for (int i = 0; i < rows; i++) {
                if (!isNewLine) {
                    line.append(CROSSING_B);
                } else {
                    isNewLine = false;
                    line.append(tabsBuilder);
                    line.append(CORNER_BL);
                }
                for (int j = 0; j < tableSizes[i] + 2 * spacing; j++) {
                    line.append(gc(BLINE));
                }
            }
            line.append(CORNER_BR);

            line.append(System.lineSeparator());
            isNewLine = true;
        }

        return line.toString();
    }

    private String gc(String[] src) {
        return src[ucode ? 1 : 0];
    }

    public static enum EnumAlignment {
        LEFT, CENTER, RIGHT
    }
}
