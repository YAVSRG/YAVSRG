namespace Interlude.Web.Server.Domain

open System.Net.Security
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates
open NRedisStack.RedisStackCommands
open StackExchange.Redis
open Interlude.Web.Server

module Redis =

    let redis: ConnectionMultiplexer = 
        let options = new ConfigurationOptions()
        options.EndPoints.Add("redis", 6379)
        options.User <- "default"
        options.Password <- "redispw"
        options.Ssl <- false
        options.SslProtocols <- SslProtocols.Tls12
        options.add_CertificateSelection(LocalCertificateSelectionCallback(fun _ _ _ _ _ -> new X509Certificate2(SECRETS.ApiCert, SECRETS.ApiCertPassword)))
        options.add_CertificateValidation(RemoteCertificateValidationCallback(fun _ _ _ _ -> true))
        ConnectionMultiplexer.Connect(options)

    let db = redis.GetDatabase()
    let ft = db.FT()
    let json = db.JSON()
            

            
        