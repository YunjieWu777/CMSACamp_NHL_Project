# PURPOSE: Flowchart displaying possible outcomes for a shot puck


# Load in packages --------------------------------------------------------

library(DiagrammeR)

grViz(diagram = "digraph flowchart {
      node [ fontname = TimesNewRoman, shape = rectangle, color = Lavender, style = filled]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      
      tab1 -> tab2;
      tab1 -> tab3;
      tab2 -> tab4;
      tab2 -> tab5;
      tab5 -> tab6;
      tab5 -> tab7;
      tab5 -> tab8;
      tab5 -> tab9;
      tab5 -> tab10;
}
      
      [1]: 'Did the Shot Hit the Net?'
      [2]: 'Shot on Net'
      [3]: 'Shot Misses Net'
      [4]: 'Goal'
      [5]: 'Not a Goal'
      [6]: 'Goalie Freezes Puck'
      [7]: 'Shot Generates Rebound'
      [8]: 'Play Continued in Zone'
      [9]: 'Play Continued Outside Zone'
      [10]: 'Stoppage of Play'
      
      ")



