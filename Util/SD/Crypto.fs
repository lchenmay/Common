module Util.Crypto

open System
open System.Numerics
open System.Collections.Generic
open System.IO
open System.Text
open System.Security.Cryptography
open BCrypt.Net

open Crypto.RIPEMD

open Util.Perf
open Util.Math
open Util.Bin

// [|"MD5"; "SHA1"; "SHA256"; "SHA384"; "SHA512"; "RIPEMD160"|]
// let sha1 = new SHA1CryptoServiceProvider();
let sha1 = SHA1.Create()
let sha256 = SHA256.Create()
let sha512 = SHA512.Create()


let sha3_256 = SHA3.Net.Sha3.Sha3256()

let bin__sha1(bin:byte[]) =
    let hash = 
        lock sha1 (fun () ->
            bin
            |> sha1.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let bin__sha256(bin:byte[]) =
    let hash = 
        lock sha256 (fun () ->
            bin
            |> sha256.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let bin__sha3_256(bin:byte[]) =
    let hash = 
        lock sha3_256 (fun () ->
            bin
            |> sha3_256.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let bin__sha256bin (bin:byte[]) =
    lock sha256 (fun () ->
        bin
        |> sha256.ComputeHash)

let bin__sha3_256bin (bin:byte[]) =
    lock sha3_256 (fun () ->
        bin
        |> sha3_256.ComputeHash)

let bin__RIPEMD160bin:byte[] -> byte[] = RIPEMD160Managed.HashInBytes

let bin__sha256PositiveBigInteger (bin:byte[]) = 
    [|  bin__sha256bin bin
        Array.zeroCreate 1 |]
    |> Array.concat

let bin__sha3_256PositiveBigInteger (bin:byte[]) = 
    [|  bin__sha3_256bin bin
        Array.zeroCreate 1 |]
    |> Array.concat



let str__sha1(s:string) =
    let hash = 
        lock sha1 (fun () ->
            s
            |> System.Text.Encoding.UTF8.GetBytes
            |> sha1.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let str__sha256(s:string) =
    let hash = 
        lock sha256 (fun () ->
            s
            |> System.Text.Encoding.UTF8.GetBytes
            |> sha256.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let str__sha3_256(s:string) =
    let hash = 
        lock sha3_256 (fun () ->
            s
            |> System.Text.Encoding.UTF8.GetBytes
            |> sha3_256.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let str__hmacsha256(k:string) (s:string) =
    let hmacsha256 = new HMACSHA256((System.Text.Encoding.ASCII.GetBytes k))
    let hash = 
        lock(hmacsha256)(fun () ->
            s
            |> System.Text.Encoding.UTF8.GetBytes
            |> hmacsha256.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let sha256rand() = 
    let s = Guid.NewGuid().ToString()
    let t = DateTime.UtcNow.Ticks.ToString()
    str__sha256(s + t)

let bin__hash(algorithm:HashAlgorithm)(bin:byte[]) = 
    let hash = 
        lock(algorithm)(fun () ->
            bin
            |> algorithm.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let str__hash(algorithm:HashAlgorithm)(s:string) = 
    let hash = 
        lock(algorithm)(fun () ->
            s
            |> System.Text.Encoding.UTF8.GetBytes
            |> algorithm.ComputeHash
            |> BitConverter.ToString)
    hash.ToUpper().Replace("-","")

let DES_encode(src:string,key:string) =
//  use des = new DESCryptoServiceProvider()
    use des = DES.Create()
    let inputByteArray = System.Text.Encoding.UTF8.GetBytes(src)
    des.Key <- ASCIIEncoding.ASCII.GetBytes(key)
    des.IV <- ASCIIEncoding.ASCII.GetBytes(key)
    let ms = new System.IO.MemoryStream()
    using(new CryptoStream(ms, des.CreateEncryptor(), CryptoStreamMode.Write))(fun cs ->
        cs.Write(inputByteArray, 0, inputByteArray.Length)
        cs.FlushFinalBlock()
        cs.Close())
    let str = Convert.ToBase64String(ms.ToArray())
    ms.Close()
    str

let DES_decode(src:string,key:string) =
    let inputByteArray = Convert.FromBase64String(src)
    // use des = new DESCryptoServiceProvider()
    use des = DES.Create()

    des.Key <- ASCIIEncoding.ASCII.GetBytes(key)
    des.IV <- ASCIIEncoding.ASCII.GetBytes(key)
    let ms = new System.IO.MemoryStream()
    using(new CryptoStream(ms, des.CreateDecryptor(), CryptoStreamMode.Write))(fun cs ->
        cs.Write(inputByteArray, 0, inputByteArray.Length)
        cs.FlushFinalBlock()
        cs.Close())

    let str = Encoding.UTF8.GetString(ms.ToArray())
    ms.Close()
    str

let to_md5data(data:string) =
    //let md5 = new MD5CryptoServiceProvider()
    let md5 = MD5.Create()
    let str = data |> Encoding.UTF8.GetBytes |>  md5.ComputeHash  |> BitConverter.ToString
    str.Replace("-","")

let get_md5data1(data:Dictionary<string,string>,key:string) =
    let mutable str= 
        data
        |> Seq.filter(fun item->  item.Value <> null && item.Value <> "" && item.Value <> "null")
        |> Seq.filter(fun item->  item.Key.ToLower() <> "sign" && item.Key.ToLower() <> "action" && item.Key.ToLower() <> "key" )
        |> Seq.sortBy(fun item-> item.Key)
        |> Seq.map(fun item->
            item.Key + "=" + item.Value )
        |> String.concat("&")
    str <- str + "&" + key
    let md5 = str |> to_md5data
    str,md5

let get_md5data(data:Dictionary<string,string>,key:string) =
    let mutable str= 
        data
        |> Seq.filter(fun item->  item.Value <> null && item.Value <> "" && item.Value <> "null")
        |> Seq.filter(fun item->  item.Key.ToLower() <> "sign" && item.Key.ToLower() <> "action" && item.Key.ToLower() <> "key")
        |> Seq.sortBy(fun item-> item.Key)
        |> Seq.map(fun item->
            item.Key + "=" + item.Value )
        |> String.concat("&")
    str <- str + "&key=" + key
    let md5 = str |> to_md5data
    str,md5

let str__bcryptHash(s:string) =
    let salt = BCrypt.GenerateSalt(10)
    BCrypt.HashPassword(s, salt)

let validateBcryptPwd(pwd:string, pwdHash:string) =
    BCrypt.Verify(pwd, pwdHash)

// wordpress password hash function
let validateWordpressPwd(pwd:string, pwdHash:string) =
    let itoa64 = "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    let wordpressEncoder(bytes: byte [], count: int): string =
        let mutable characters = itoa64.Length
        let mutable result = ""
        let mutable i = 0

        while i < count do
            let mutable value = int bytes.[i]
            i <- i + 1
            result <- result + itoa64.[value % characters].ToString()
            if i < count then
                value <-  value ||| (int bytes[i] <<< 8)
            result <- result + itoa64.[(value >>> 6) % characters].ToString()

            if i < count then
                i <- i + 1
                if i < count then
                    value <- value ||| (int bytes[i] <<< 16)
                result <- result + itoa64.[(value >>> 12) % characters].ToString()
                if i < count then
                    i <- i + 1
                    result <- result + itoa64.[(value >>> 18) % characters].ToString()
        result

    let encrypt =
        let count_log2 = itoa64.IndexOf(pwdHash[3])
        let md5 = MD5.Create()
        let mutable hashRounds = 1 <<< count_log2
        let mutable hashBytes = (pwdHash.Substring(4, 8) + pwd) |> Encoding.UTF8.GetBytes |> md5.ComputeHash
        let pwdBytes = pwd |> Encoding.UTF8.GetBytes

        let mutable previousHashBytes = Array.zeroCreate<byte>(hashBytes.Length + pwdBytes.Length)
        Buffer.BlockCopy(hashBytes, 0, previousHashBytes, 0, hashBytes.Length)
        Buffer.BlockCopy(pwdBytes, 0, previousHashBytes, hashBytes.Length, pwdBytes.Length)

        for i = 0 to (hashRounds - 1) do
            hashBytes <- previousHashBytes |>  md5.ComputeHash 
            Array.Copy(hashBytes, previousHashBytes, hashBytes.Length);

        pwdHash.Substring(0, 12) + wordpressEncoder(hashBytes, hashBytes.Length)
        
    if (pwd = null || pwdHash = null || pwd.Length * pwdHash.Length = 0 || pwdHash.Length < 34) then
        false
    else
        encrypt.Equals(pwdHash)

let str__uint64hash adics (src:string) =
    let bytes = 
        lock(sha1)(fun () ->
            src
            |> System.Text.Encoding.UTF8.GetBytes
            |> sha1.ComputeHash)
    let sub = Array.sub bytes (bytes.Length - 8) 8
    System.BitConverter.ToUInt64 sub

let uint64__str (alphabet:string) adics v = 
    let padic = Util.Math.uint64__adic adics v
    let cs = 
        padic 
        |> Array.map(fun i -> alphabet.[int i])
    new string(cs)

let str__uint64 (alphabet:string) adics (s:string) = 
    [| 0 .. s.Length - 1 |]
    |> Array.map(fun i -> uint64(alphabet.IndexOf s.[i]))
        

let data__HashedBin data__bin = 
        
    let bb = new BytesBuilder()

    data__bin bb

    bb.bytes()
    |> bin__sha256bin
    |> bb.insert 

    bb.bytes()

    
let HashedBin__datao bin__data (bin:byte[]) = 
    let mutable o = None
    if bin.Length > 32 then
        let hash = bin |> Array.take 32
        let payload = Array.sub bin 32 (bin.Length - 32)
        if  payload
            |> bin__sha256bin
            |> Array.compareWith (fun i j -> i.CompareTo j) hash = 0 then
            let index = ref 0
            o <-
                bin__data(payload,index)
                |> Some 
    o

// RSA
// ===============================================================

let RSA = new RSACryptoServiceProvider()
let publicKey = RSA.ExportRSAPublicKey()
let publicKeyStr = Convert.ToBase64String publicKey
let privateKey = RSA.ExportRSAPrivateKey()

let msgEncrypt(input: byte[], publicki: byte[]) =
    try
        use rsa = new RSACryptoServiceProvider()       
        rsa.ImportRSAPublicKey(publicki, ref publicki.Length)
        let MaxBlockSize = rsa.KeySize / 8 - 11
        let mutable encrypted = [||]
        if input.Length <= MaxBlockSize then
            encrypted <- rsa.Encrypt(input, false)
        else
            use inputStream = new MemoryStream(input)
            use outputStream = new MemoryStream()
            let buff: byte[] = Array.zeroCreate MaxBlockSize
            let mutable nReadBytes = inputStream.Read buff
            while nReadBytes > 0 do
                let bytes = Array.sub buff 0 nReadBytes
                let out = rsa.Encrypt(bytes, false)
                outputStream.Write(out, 0, out.Length)
                nReadBytes <- inputStream.Read buff
            encrypted <- outputStream.ToArray()
        encrypted
    with ex ->
        Console.WriteLine(ex.Message)
        [||]

let msgDecrypt(input: byte[], privateki: byte[]) =    
    try        
        let rsa = new RSACryptoServiceProvider()
        rsa.ImportRSAPrivateKey(privateki, ref privateki.Length)
        let MaxBlockSize = rsa.KeySize / 8
        let mutable decrypted = [||]
        if input.Length <= MaxBlockSize then
            decrypted <- rsa.Decrypt(input, false)
        else
            let inputStream = new MemoryStream(input)
            let outputStream = new MemoryStream()
            let buff: byte[] = Array.zeroCreate MaxBlockSize
            let mutable nReadBytes = inputStream.Read(buff)
            while nReadBytes > 0 do
                let bytes = Array.sub buff 0 nReadBytes
                let out = rsa.Decrypt(bytes, false)
                outputStream.Write(out, 0, out.Length)
                nReadBytes <- inputStream.Read(buff)
            decrypted <- outputStream.ToArray()
            inputStream.Dispose()
            outputStream.Dispose()
        rsa.Dispose()
        decrypted
    with ex ->
        Console.WriteLine(ex.ToString())
        [||]    


// ECC
// ===============================================================

let hex__bigint (hex:string) = 
    hex.Replace(" ","").ToUpper().ToCharArray()
    |> Array.windowed 2
    |> Array.mapi (fun i j -> (i,j))
    |> Array.filter (fun (i,j) -> i % 2=0)
    |> Array.map (fun (_,j) -> Byte.Parse(new System.String(j),System.Globalization.NumberStyles.AllowHexSpecifier))
    |> Array.ofSeq
    |> bin__bigint256

let bigint__hex (bigint:BigInteger) = 
    bigint.ToByteArray(true,true)
    |> Array.fold (fun state x -> state + sprintf "%02X" x) ""


let ecPoint__bin bb ecp = 
    ecp |> fst |> bigint__bin bb
    ecp |> snd |> bigint__bin bb
let bin__ecPoint bi = bin__bigint bi,bin__bigint bi

let ecPoint__binKey ecp = 
    let bb = new BytesBuilder()
    ecPoint__bin bb ecp
    bb.bytes()


type EllipticCurveOverGaloisField = {
    a: BigInteger
    b: BigInteger
    p: BigInteger
    G: ecPoint
    n: BigInteger
    table: ecPoint[] }

let ecgPointValidate (ec:EllipticCurveOverGaloisField) ecp = 
    if ecPoint__infy ecp then
        true
    else
        let x,y = ecp
        x * x * x + ec.a * x + ec.b - y * y % ec.p = BigInteger.Zero

let ecgPoint__json (ecp:ecPoint) = 
    let x,y = ecp
    "{\"x\":\"" + (x |> bigint__hex) + "\",\"y\":\"" + (y |> bigint__hex) + "\"}"

let rx = @"(?<=\x22x\x22:\x22)\d+" |> Text.string__regex
let ry = @"(?<=\x22y\x22:\x22)\d+" |> Text.string__regex

let json__ecgPoint json = 
    let sx = Util.Text.find ("\"x\":\"","\",\"") json
    let sy = Util.Text.find ("\"y\":\"","\"}") json
    hex__bigint sx, hex__bigint sy

let ecTable256 p a G = 

    let mutable current = G

    [| 0 .. 256 |]
    |> Array.map(fun i -> 
        if i = 0 then
            G
        else
            current <- ecGaloisAdd p a (current,current)
            current)

let secp256k1 = 
    
    let a = BigInteger.Zero
    let b = BigInteger 7

    let G = 
        // x = 55066263022277343669578718895168534326250603453777594175500187360389116729240
        // y = 32670510020758816978083085130507043184471273380659243275938904335757337482424
        let x = hex__bigint "79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798"
        let y = hex__bigint "483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8"
        x,y
            
    let p = hex__bigint "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2F"

    let n = 
        // n = 115792089237316195423570985008687907852837564279074904382605163141518161494337
        hex__bigint "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141" 
        
    let table = ecTable256 p a G
                
    { a = a; b = b; p = p; G = G; n = n; table = table }

let ec__privateKey256 ec = BigInteger.One + Prob.rand__bigint256 (ec.n - BigInteger.One)

let scalar256 p a (table:ecPoint[]) (s:BigInteger) =

    use cw = new CodeWrapper("Util.Crypto.scalar256")

    let e2s = e2s256

    let mutable o = None
    let mutable v = s

    [| 0 .. 256 |]
    |> Array.iter(fun i -> 
        let index = 256 - i
        let m = e2s[index]
        if v / m = BigInteger.One then
            o <- 
                match o with
                | Some pt -> ecGaloisAdd p a (pt,table[index])
                | None -> table[index]
                |> Some
        v <- v % m)

    o.Value

// http://kjur.github.io/jsrsasign/sample/sample-ecdsa.html
let ecScalar256 ec = scalar256 ec.p ec.a ec.table

// http://kjur.github.io/jsrsasign/sample/sample-ecdsa.html
let private__public ec privateKey = 
    let x,y = ecScalar256 ec privateKey
    System.Console.WriteLine ("x:" + x.ToString())
    System.Console.WriteLine ("y:" + y.ToString())       
 
    let sx = x |> bigint__hex
    let sy = y |> bigint__hex
    (x,y),"0x" + sx + sy

type SignedBin = byte[] * (BigInteger * BigInteger)

// https://learnblockchain.cn/article/1551
let ecgSign ec privateKey bin = 

    use cw = new CodeWrapper("Util.Crypto.ecgSign")
        
    let mutable k = BigInteger.Zero
    let mutable r = BigInteger.Zero
    let mutable s = BigInteger.Zero

    let generate1() = 
        r <- BigInteger.Zero
        while r = BigInteger.Zero do
            k <- BigInteger.One + Prob.rand__bigint256 (ec.n - BigInteger.One)
            let rx,ry = ecScalar256 ec k
            r <- rx % ec.n

    let generate2() = 
        s <- BigInteger.Zero
        while s = BigInteger.Zero do
            generate1()
            let H = new BigInteger(bin |> bin__sha256PositiveBigInteger)
            s <- ((H + r * privateKey) * (gfInv ec.n k)) % ec.n

    generate2()

    let rhex = bigint__hex r
    let shex = bigint__hex s

    bin,(r,s)

let ecgValidate ec publicKey signedBin = 
        
    use cw = new CodeWrapper("Util.Crypto.ecgValidate")

    let bin,(r,s) = signedBin
        
    let bytes = 
        bin 


    let H = new BigInteger(bin |> bin__sha256PositiveBigInteger)
    let u1 = (H * (gfInv ec.n s)) % ec.n
    let u2 = (r * (gfInv ec.n s)) % ec.n
    let r1 = scalar256 ec.p ec.a (ecTable256 ec.p ec.a ec.G) u1
    let r2 = scalar256 ec.p ec.a (ecTable256 ec.p ec.a publicKey) u2
    let rx,ry = ecGaloisAdd ec.p ec.a (r1,r2)
    let v = rx % ec.n

    v = r

// AES
// ===============================================================
let aesEncrypt (input: string) (key: byte array) (iv: byte array) =
    try
        use encryptor = Aes.Create()
        encryptor.Key <- key
        encryptor.IV <- iv
        let msEncrypt = new MemoryStream()
        let csEncrypt = new CryptoStream(msEncrypt, encryptor.CreateEncryptor(), CryptoStreamMode.Write)
        let swEncrypt = new StreamWriter(csEncrypt)
        swEncrypt.Write(input)
        swEncrypt.Close()
        msEncrypt.ToArray() 
    with
    | _ -> [||]

let aesDecrypt (cipherText: byte array) (key: byte array) (iv: byte array) =
    try
        use decryptor = Aes.Create()
        decryptor.Key <- key
        decryptor.IV <- iv
        let msDecrypt = new MemoryStream(cipherText)
        let csDecrypt = new CryptoStream(msDecrypt, decryptor.CreateDecryptor(), CryptoStreamMode.Read)
        let srDecrypt = new StreamReader(csDecrypt)
        srDecrypt.ReadToEnd()
    with
    | _ -> ""

let keyPadding (key:string) =
    let k = key.Trim()
    if k.Length > 16 then
        k.Substring(0,16)
    elif k.Length < 16 then
        Array.init (16-k.Length) (fun _ -> "0") |> String.concat "" |> fun s -> k + s
    else
        k

let base64__aesDecrypt (keyIV:string) (base64Text:string) =
    try
        let keyIV = keyIV |> keyPadding |> Encoding.UTF8.GetBytes
        base64Text
        |> Convert.FromBase64String
        |> aesDecrypt
        <|| (keyIV, keyIV)
    with
    | _ -> base64Text

let aesEncrypt__base64 (keyIV:string) (input:string) =
    try
        let keyIV = keyIV |> keyPadding |> Encoding.UTF8.GetBytes
        input
        |> aesEncrypt
        <|| (keyIV, keyIV)
        |> Convert.ToBase64String
    with
    | _ -> input
