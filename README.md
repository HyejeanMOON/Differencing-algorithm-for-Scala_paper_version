# Differencing-algorithm-for-Scala_paper_version

This project is based on call graph for scala(scalacg).  
link: https://plg.uwaterloo.ca/~karim/projects/scalacg/  
Express my gratitude for MR.Karim.

Paper:  
Title: Scala向けの差分解析アルゴリズム  
Gakkai: 情報処理学会　全国大会第80回  

### How to run it?
'$ scalac -cp callgraph-plugin.jar -Xplugin:plugin-name.jar [-P:callgraph:<analysis>] <code.scala>'   
  
 You will run plugin twice.First is for original program(project), twice is for modified program(project).
 Then, you can analysis results by another version of differencing_algorithm_for_scala.
  
