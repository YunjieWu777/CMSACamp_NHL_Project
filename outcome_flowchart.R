# PURPOSE: Flowchart displaying possible outcomes for a shot puck


# Load in packages --------------------------------------------------------

library(DiagrammeR)

grViz("
      digraph boxes_and_circles {

      # a 'graph' statement
      graph [overlap = true, fontsize = 10]

      # several 'node' statements
      node [shape = box,
      fontname = Helvetica]
      A; B; C; D; E; F

      node [shape = circle,
      fixedsize = true,
      width = 0.9] // sets as circles
      1; 2; 3; 4; 5; 6; 7; 8

      # several 'edge' statements
      A->1 [label='YES']
      B->2 [label='NO'] 
      B->3 [label='...'] 
      B->4 C->A
      1->D E->A 2->4 1->5 1->F
      E->6 4->6 5->7 6->7 3->8
      }
")

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
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      
      tab1 -> tab2;
      tab1 -> tab3;
      tab3 -> tab4;
      tab3 -> tab5;
      tab2 -> tab6;
      tab2 -> tab7;
      tab7 -> tab8;
      tab7 -> tab9;
      tab7 -> tab10;
      tab7 -> tab11;
      tab7 -> tab12;
}
      
      [1]: 'Did the Shot Hit the Net?'
      [2]: 'Shot on Net'
      [3]: 'Shot Misses Net'
      [4]: 'Play Continued in Zone'
      [5]: 'Play Continued Outside Zone'
      [6]: 'Goal'
      [7]: 'Not a Goal'
      [8]: 'Goalie Freezes Puck'
      [9]: 'Shot Generates Rebound'
      [10]: 'Play Continued in Zone'
      [11]: 'Play Continued Outside Zone'
      [12]: 'Stoppage of Play'
      
      ")






