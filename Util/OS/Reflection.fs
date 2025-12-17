module Util.Reflection

open LanguagePrimitives

open System
open System.IO
open System.Reflection

    

/// Handles the AssemblyResolve event to locate dependencies 
/// in the same directory as the target assembly.
let resolveDependencies output (targetAssemblyPath: string) (sender: obj) (args: ResolveEventArgs) =
    // 1. Get the directory of the assembly we are trying to load
    let assemblyDirectory = Path.GetDirectoryName(targetAssemblyPath)
    
    // 2. Construct the full path to the missing dependency
    let requestedAssemblyName = args.Name.Split(',') |> Array.head // Get simple name (e.g., "Newtonsoft.Json")
    let dependencyPath = Path.Combine(assemblyDirectory, requestedAssemblyName + ".dll")

    $"Attempting to resolve dependency: {args.Name}" |> output
    $"Looking for file: {dependencyPath}" |> output

    // 3. Try to load the dependency
    if File.Exists(dependencyPath) then
        try
            // Load the assembly and return it to the CLR
            let resolvedAssembly = Assembly.LoadFrom(dependencyPath)
            $"Successfully resolved and loaded: {resolvedAssembly.FullName}" |> output
            resolvedAssembly
        with ex ->
            $"Failed to load dependency {dependencyPath}: {ex.Message}" |> output
            null // Return null to indicate failure
    else
        "Dependency not found at expected path." |> output
        null // Return null to indicate failure

// --- Main Loading Function ---
let tryLoadFromFileWithDependencies output (dllPath: string) =
    
    "try loading: " + dllPath
    |> output
    
    let assemblyDirectory = Path.GetDirectoryName(dllPath)
    
    // Install the resolver BEFORE attempting to load the assembly
    let resolver = new ResolveEventHandler(fun s a -> 
        resolveDependencies output dllPath s a)
    AppDomain.CurrentDomain.add_AssemblyResolve(resolver)
    
    try
        let assembly = Assembly.LoadFrom dllPath

        // --- Critical Step for ReflectionTypeLoadException ---
        // Force the CLR to resolve all types while the resolver is active
        [|  "SUCCESS ["
            assembly.FullName 
            "]: "
            assembly.GetTypes().Length.ToString()
            " types found" |]
        |> String.Concat
        |> output 
        
        // Remove the resolver once done to prevent side effects
        AppDomain.CurrentDomain.remove_AssemblyResolve(resolver)
        
        Some assembly

    with
    | :? ReflectionTypeLoadException as rtle ->
        // Even with the resolver, if something is still missing, inspect it here
        
        [|  "FAIL ["
            + dllPath
            "]: "
            "unresolved dependencies, ReflectionTypeLoadException loading. " 
            "rtle.LoaderExceptions = " + rtle.LoaderExceptions.Length.ToString() |]
        |> String.Concat
        |> output

        rtle.LoaderExceptions
        |> Array.map(fun e -> e.ToString())
        |> Array.iter output

        AppDomain.CurrentDomain.remove_AssemblyResolve(resolver)
        
        None
    | ex ->
        AppDomain.CurrentDomain.remove_AssemblyResolve(resolver)
        
        "FAIL [" + dllPath + "]"
        |> output
        
        ex.Message
        |> output

        None