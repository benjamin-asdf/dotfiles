@echo off
setlocal

IF NOT EXIST IdleGame.sln (
   echo place this in the IdleGame dir
   exit \B 1
)

SET outputpath=%TMP%/unity_object_closures.txt
SET slnpath=%~dp0IdleGame.sln

.\Tools\RoslynAnalyzers\EntityClosureCLI\EntityClosureCLI.exe -s "%slnpath%" -x "(Test)|(^Unity\.)|(WIP)|(Editor)|(Plugins)|(TMPro)|(Assembly)|(Monkeys)" -i ".*\\Assets\\.*" -a "UnityObjectClosureAnalyzer" -o "%outputpath%"
&& code %outputpath% || echo failed to open %outputpath% with vs code && pause
