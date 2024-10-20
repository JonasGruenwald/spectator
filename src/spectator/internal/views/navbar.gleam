//// View functions for rendering the top navigation bar

import lustre/attribute.{attribute}
import lustre/element/html
import lustre/element/svg

fn icon() {
  svg.svg(
    [
      attribute("class", "lucy-icon"),
      attribute("xmlns:bx", "https://boxy-svg.com"),
      attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute("fill", "none"),
      attribute("viewBox", "-40.546 -53.577 2251.673 2524.133"),
    ],
    [
      svg.path([
        attribute("transform", "matrix(-1, 0, 0, -1, 0.000033, 0.000056)"),
        attribute(
          "style",
          "transform-box: fill-box; transform-origin: 50% 50%;",
        ),
        attribute("fill", "#FFAFF3"),
        attribute(
          "d",
          "M 842.763 1885.449 C 870.851 1965.043 975.268 1983.454 1028.887 1918.269 L 1309.907 1576.606 C 1344.677 1534.33 1396.137 1509.279 1450.877 1507.946 L 1893.517 1497.176 C 1978.027 1495.121 2027.657 1401.679 1982.187 1330.643 L 1743.607 957.976 C 1729.017 935.196 1719.507 909.536 1715.737 882.746 C 1711.957 855.966 1714.007 828.676 1721.737 802.756 L 1848.077 378.926 C 1872.157 298.176 1798.617 221.916 1716.767 243.186 L 1288.287 354.516 C 1262.097 361.326 1234.747 362.416 1208.097 357.716 C 1181.457 353.026 1156.127 342.646 1133.847 327.286 L 769.294 76.116 C 699.65 28.146 604.466 74.646 599.463 158.766 L 573.239 600.246 C 569.994 654.886 543.11 705.386 499.603 738.616 L 147.94 1007.216 C 80.914 1058.41 95.596 1163.196 174.303 1194.032 L 586.57 1355.546 C 637.55 1375.519 677.336 1416.659 695.55 1468.278 L 842.763 1885.449 Z",
        ),
      ]),
      svg.path([
        attribute("transform", "matrix(-1, 0, 0, -1, 0.000013, -0.000111)"),
        attribute(
          "style",
          "transform-box: fill-box; transform-origin: 50% 50%;",
        ),
        attribute("fill", "#151515"),
        attribute(
          "d",
          "M 918.91 1994.668 C 868.969 1985.861 823.32 1952.703 804.42 1899.152 L 657.186 1481.986 C 642.831 1441.315 611.52 1408.933 571.327 1393.185 L 159.044 1231.661 C 53.25 1190.213 32.792 1044.7 122.981 975.816 L 474.644 707.246 C 491.576 694.326 505.522 677.906 515.528 659.096 C 525.534 640.296 531.366 619.556 532.625 598.296 L 558.835 156.826 C 565.559 43.566 697.668 -20.864 791.287 43.616 L 791.289 43.626 L 1155.87 294.796 L 1155.87 294.806 C 1173.42 306.906 1193.38 315.086 1214.37 318.786 C 1235.37 322.486 1256.92 321.626 1277.55 316.256 L 1706.04 204.946 C 1816.06 176.356 1918.17 282.096 1885.75 390.836 L 1885.75 390.826 L 1759.42 814.646 C 1753.33 835.056 1751.72 856.536 1754.69 877.636 C 1757.66 898.726 1765.15 918.926 1776.65 936.856 L 1776.65 936.866 L 2015.25 1309.543 L 2015.24 1309.545 C 2076.44 1405.123 2007.46 1534.859 1893.87 1537.622 L 1451.21 1548.375 C 1408.06 1549.423 1367.56 1569.142 1340.17 1602.448 L 1059.15 1944.101 C 1023.08 1987.955 968.841 2003.497 918.896 1994.689 M 932.34 1918.443 C 955.035 1922.445 979.819 1914.605 997.365 1893.277 L 1278.38 1551.634 C 1320.52 1500.393 1382.95 1470.014 1449.26 1468.401 L 1891.92 1457.648 C 1947.35 1456.302 1977.63 1399.16 1947.86 1352.674 L 1709.27 979.996 C 1673.48 924.126 1663.8 855.366 1682.74 791.796 L 1809.08 367.986 C 1824.81 315.206 1779.83 268.436 1726.15 282.376 L 1297.66 393.696 C 1233.45 410.386 1165.09 398.326 1110.46 360.676 L 745.884 109.506 C 700.205 78.036 641.945 106.606 638.676 161.576 L 612.469 603.046 C 608.537 669.276 575.914 730.556 523.186 770.836 L 171.524 1039.406 C 127.662 1072.905 136.58 1136.956 188.203 1157.189 L 600.485 1318.715 C 662.256 1342.917 710.525 1392.816 732.608 1455.385 L 732.608 1455.387 L 879.842 1872.552 C 889.034 1898.593 909.64 1914.438 932.335 1918.441",
        ),
      ]),
      svg.path([
        attribute("transform", "matrix(-1, 0, 0, -1, -0.000262, 0.00008)"),
        attribute(
          "style",
          "transform-box: fill-box; transform-origin: 50% 50%;",
        ),
        attribute("fill", "#151515"),
        attribute(
          "d",
          "M 1340.734 989.33 C 1383.47 996.87 1412.007 1037.62 1404.473 1080.35 C 1396.939 1123.09 1356.188 1151.617 1313.452 1144.081 C 1270.716 1136.544 1242.179 1095.79 1249.713 1053.06 C 1257.247 1010.33 1297.999 981.79 1340.734 989.33 Z",
        ),
      ]),
      svg.path([
        attribute("transform", "matrix(-1, 0, 0, -1, 0.000102, -0.000003)"),
        attribute(
          "style",
          "transform-box: fill-box; transform-origin: 50% 50%;",
        ),
        attribute("fill", "#151515"),
        attribute(
          "d",
          "M 707.68 877.704 C 750.41 885.234 778.95 925.99 771.41 968.722 C 763.88 1011.455 723.13 1039.988 680.39 1032.451 C 637.66 1024.915 609.12 984.163 616.65 941.43 C 624.19 898.694 664.94 870.164 707.68 877.704 Z",
        ),
      ]),
      svg.path([
        attribute("transform", "matrix(-1, 0, 0, -1, 0.000158, -0.000137)"),
        attribute(
          "style",
          "transform-box: fill-box; transform-origin: 50% 50%;",
        ),
        attribute("fill", "#151515"),
        attribute(
          "d",
          "M 908.965 1220.696 C 904.065 1218.806 899.585 1215.966 895.775 1212.346 C 891.975 1208.726 888.915 1204.386 886.795 1199.586 C 884.665 1194.776 883.505 1189.606 883.375 1184.356 C 883.245 1179.106 884.155 1173.876 886.045 1168.976 C 890.985 1156.206 898.385 1144.526 907.835 1134.606 C 917.285 1124.686 928.595 1116.736 941.125 1111.196 L 941.135 1111.186 C 953.645 1105.646 967.125 1102.626 980.815 1102.296 L 980.825 1102.296 L 980.845 1102.296 C 994.525 1101.976 1008.135 1104.346 1020.905 1109.276 L 1020.915 1109.276 C 1033.685 1114.206 1045.365 1121.606 1055.275 1131.056 L 1055.285 1131.056 L 1055.285 1131.066 C 1065.195 1140.506 1073.145 1151.816 1078.685 1164.326 C 1084.245 1176.856 1087.265 1190.366 1087.585 1204.066 C 1087.835 1214.666 1083.865 1224.936 1076.545 1232.616 C 1069.215 1240.286 1059.145 1244.736 1048.535 1244.986 C 1043.285 1245.116 1038.065 1244.196 1033.165 1242.306 C 1028.265 1240.406 1023.785 1237.566 1019.985 1233.936 C 1016.185 1230.316 1013.135 1225.976 1011.015 1221.166 C 1008.895 1216.366 1007.735 1211.186 1007.615 1205.936 C 1007.535 1202.766 1006.835 1199.636 1005.545 1196.736 L 1005.545 1196.726 L 1005.535 1196.706 C 1004.245 1193.796 1002.395 1191.166 1000.095 1188.966 C 997.785 1186.766 995.075 1185.046 992.105 1183.906 L 992.085 1183.896 C 989.105 1182.746 985.935 1182.196 982.745 1182.266 C 979.565 1182.346 976.425 1183.056 973.515 1184.346 L 973.495 1184.346 L 973.485 1184.356 C 970.575 1185.646 967.955 1187.486 965.755 1189.786 L 965.755 1189.796 C 963.555 1192.106 961.835 1194.816 960.685 1197.796 C 958.795 1202.696 955.955 1207.176 952.335 1210.976 C 948.705 1214.776 944.375 1217.836 939.565 1219.956 C 934.765 1222.086 929.585 1223.246 924.335 1223.376 C 919.085 1223.506 913.865 1222.596 908.965 1220.696 Z",
        ),
      ]),
      svg.path([
        attribute(
          "d",
          "M 837.332 1799.273 L 1040.536 1392.732 L 1200.981 1417.848 L 1168.502 1900.5",
        ),
        attribute("style", "fill: rgb(216, 216, 216); stroke: rgb(0, 0, 0);"),
      ]),
      svg.path([
        attribute(
          "d",
          "M 1462.463 1851.433 L 1464.166 1415.159 L 1635.88 1406.161 L 1804.306 1841.839",
        ),
        attribute("style", "fill: rgb(216, 216, 216); stroke: rgb(0, 0, 0);"),
      ]),
      svg.g(
        [
          attribute(
            "transform",
            "matrix(5.468584, 0, 0, 5.468584, -10849.720703, -6934.962402)",
          ),
          attribute("style", ""),
        ],
        [
          svg.path([
            attribute("style", "paint-order: fill; fill: rgb(255, 251, 232);"),
            attribute("opacity", "0.2"),
            attribute(
              "d",
              "M 2200.077 1640.391 C 2200.077 1671.183 2166.744 1690.428 2140.077 1675.032 C 2113.41 1659.636 2113.41 1621.146 2140.077 1605.75 C 2146.158 1602.239 2153.055 1600.391 2160.077 1600.391 C 2182.168 1600.391 2200.077 1618.3 2200.077 1640.391 Z M 2288.077 1600.391 C 2257.285 1600.391 2238.04 1633.724 2253.436 1660.391 C 2268.832 1687.058 2307.322 1687.058 2322.718 1660.391 C 2326.229 1654.31 2328.077 1647.412 2328.077 1640.391 C 2328.077 1618.3 2310.168 1600.391 2288.077 1600.391 Z",
            ),
          ]),
          svg.path([
            attribute("style", "fill: rgb(47, 47, 47);"),
            attribute(
              "d",
              "M 2333.277 1624.261 C 2332.609 1622.397 2331.824 1620.576 2330.927 1618.811 L 2289.337 1524.191 C 2288.943 1523.28 2288.38 1522.452 2287.677 1521.751 C 2275.18 1509.251 2254.914 1509.251 2242.417 1521.751 C 2240.922 1523.248 2240.081 1525.276 2240.077 1527.391 L 2240.077 1552.391 L 2208.077 1552.391 L 2208.077 1527.391 C 2208.079 1525.269 2207.237 1523.233 2205.737 1521.731 C 2193.24 1509.231 2172.974 1509.231 2160.477 1521.731 C 2159.774 1522.432 2159.21 1523.26 2158.817 1524.171 L 2117.227 1618.791 C 2116.33 1620.556 2115.545 1622.377 2114.877 1624.241 C 2102.441 1659.036 2132.336 1694.245 2168.687 1687.617 C 2191.501 1683.457 2208.079 1663.581 2208.077 1640.391 L 2208.077 1568.391 L 2240.077 1568.391 L 2240.077 1640.391 C 2240.058 1677.341 2280.047 1700.456 2312.056 1681.997 C 2332.152 1670.408 2341.084 1646.106 2333.277 1624.261 Z M 2172.787 1532.141 C 2178.268 1527.526 2186.148 1527.118 2192.077 1531.141 L 2192.077 1604.651 C 2179.365 1593.237 2161.537 1589.457 2145.287 1594.731 L 2172.787 1532.141 Z M 2160.077 1672.391 C 2135.443 1672.391 2120.047 1645.724 2132.364 1624.391 C 2144.681 1603.058 2175.473 1603.058 2187.79 1624.391 C 2190.598 1629.256 2192.077 1634.774 2192.077 1640.391 C 2192.077 1658.064 2177.75 1672.391 2160.077 1672.391 Z M 2256.077 1531.131 C 2262.006 1527.108 2269.886 1527.516 2275.367 1532.131 L 2302.867 1594.711 C 2286.615 1589.44 2268.787 1593.223 2256.077 1604.641 L 2256.077 1531.131 Z M 2288.077 1672.391 C 2263.443 1672.391 2248.047 1645.724 2260.364 1624.391 C 2272.681 1603.058 2303.473 1603.058 2315.79 1624.391 C 2318.598 1629.256 2320.077 1634.774 2320.077 1640.391 C 2320.077 1658.064 2305.75 1672.391 2288.077 1672.391 Z",
            ),
          ]),
        ],
      ),
    ],
  )
}

fn tab(title: String, current: String, path: String) {
  let classes = case title == current {
    True -> "tab active"
    False -> "tab"
  }

  case path {
    "" ->
      html.div([attribute.class(classes <> " disabled")], [html.text(title)])
    _ ->
      html.a([attribute.class(classes), attribute.href(path)], [
        html.text(title),
      ])
  }
}

pub fn render(current_tab: String) {
  html.nav([attribute.class("topbar")], [
    html.div([attribute.class("logo")], [icon(), html.text("Spectator")]),
    html.div([attribute.class("filler")], []),
    html.div([attribute.class("tabs")], [
      tab("Processes", current_tab, "/processes"),
      html.div([attribute.class("separator")], []),
      tab("ETS", current_tab, "/ets"),
      html.div([attribute.class("separator")], []),
      tab("Ports", current_tab, ""),
      html.div([attribute.class("separator")], []),
      tab("Stats", current_tab, ""),
    ]),
    html.div([attribute.class("filler")], []),
  ])
}
