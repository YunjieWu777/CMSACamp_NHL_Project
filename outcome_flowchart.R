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
      tab8 [label = '@@7']
      
      
      
      tab1 -> tab2;
      tab1 -> tab3;
      tab3 -> tab4;
      tab3 -> tab5;
      tab3 -> tab6;
      tab3 -> tab7;
      tab3 -> tab8;
     
}
      
      [1]: 'Shot Attempt'
      [2]: 'Shot On Goal'
      [3]: 'Shot Misses Net'
      [4]: 'Shot Generates Rebound'
      [5]: 'Goalie Froze Puck'
      [6]: 'Play Continued in Zone'
      [7]: 'Play Continued Outside Zone'
      [8]: 'Stoppage of Play'
      
      ")



