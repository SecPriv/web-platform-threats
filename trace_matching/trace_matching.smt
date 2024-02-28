;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(declare-datatype Ctx
 ((ctx (ctx-id String) (ctx-location String) (ctx-secure-context Bool))))

(declare-datatypes ((Option 1))
 ((par (X) ((some (from-some X))
            (none)))))

(declare-datatype RequestHeaders
 ((request-headers (rq-hd-cookie (Option String)) (rq-hd-origin (Option String)))))

(declare-datatype ResponseHeaders
 ((response-headers (rp-hd-set-cookie (List String)))))

(declare-datatype SameSite
 ((SS-Strict) (SS-Lax) (SS-None)))

(declare-datatype CookieAttrs
  ((cookie-attrs (ca-secure Bool)
                 (ca-http-only Bool)
                 (ca-same-site SameSite)
                 (ca-path String)
                 (ca-domain String))))

(declare-datatype RequestMethod
 ((M-GET) (M-POST) (M-PUT) (M-DELETE) (M-OPTIONS) (M-PATCH) (M-OTHER)))

(declare-datatype Url
  ((build-url (u-href String)
              (u-host String)
              (u-hostname String)
              (u-origin String)
              (u-path String)
              (u-port String)
              (u-proto String)
              (u-site String))))

; https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/API/webRequest/ResourceType
(declare-datatype ResourceType
  ((type-beacon) (type-csp_report) (type-font) (type-image) (type-imageset)
   (type-main_frame) (type-media) (type-object) (type-object_subrequest)
   (type-ping) (type-script) (type-speculative) (type-stylesheet)
   (type-sub_frame) (type-web_manifest) (type-websocket) (type-xbl)
   (type-xml_dtd) (type-xmlhttprequest) (type-xslt) (type-other)))

(declare-datatype Action
  ((js-set-cookie (jsc-ctx Ctx)
                  (jsc-set String)
                  (jsc-res String))

   (js-get-cookie (jgc-ctx Ctx)
                  (jgc-res String))

   (cookie-jar-set (cjs-name String)
                   (cjs-value String)
                   (cjs-attrs CookieAttrs))

   (net-request (rq-id String)
                (rq-url String)
                (rq-method RequestMethod)
                (rq-type ResourceType)
                (rq-originurl String)
                (rq-documenturl (Option String))
                (rq-frame-ancestors (List String))
                (rq-redirections (List String))
                (rq-headers RequestHeaders)
                (rq-body (Option String)))

   (net-response (rp-id String)
                 (rp-url String)
                 (rp-headers ResponseHeaders)
                 (rp-body (Option String)))

   (js-fetch (jf-ctx Ctx)
             (jf-target String))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils


(declare-fun parse-url (String) Url)
(declare-fun domain-site (String) String)
(declare-fun split-cookie (String) (List String))

(define-fun-rec in ((l (List String)) (a String)) Bool
  (match l
    ((nil false)
     ((insert b bs) (or (= a b) (in bs a))))))

(define-fun-rec at ((n Int) (l (List Action)) (a Action)) Bool
  (and (>= n 0)
       (match l
        ((nil false)
         ((insert b bs) (or (and (= n 0) (= b a))
                            (and (> n 0) (at (- n 1) bs a))))))))

(define-fun-rec take ((n Int) (tr (List Action))) (List Action)
            (ite (<= n 0) (as nil (List Action))
                 (match tr ((nil (as nil (List Action)))
                            ((insert x xs) (insert x (take (- n 1) xs)))))))

(define-fun-rec appnd ((L (List String)) (L1 (List String))) (List String)
  (match L
    ((nil L1)
     ((insert X Xs) (insert X (appnd Xs L1))))))

(define-fun-rec rev ((L (List String))) (List String)
  (match L
    ((nil (as nil (List String)))
     ((insert L Xs) (appnd (rev Xs) (insert L (as nil (List String))))))))

(define-fun is-substring ((S1 String) (S2 String)) Bool
  (exists ((Pre String) (Post String))
    (= (str.++ Pre S1 Post) S2)))

(define-fun is-prefix ((S1 String) (S2 String)) Bool
  (exists ((Post String))
    (= (str.++ S1 Post) S2)))

(define-fun url-host ((url String) (host String)) Bool
    (= (u-host (parse-url url)) host))

(define-fun url-domain ((url String) (domain String)) Bool
    (= (u-hostname (parse-url url)) domain))

(define-fun url-site ((url String) (site String)) Bool
    (= (u-site (parse-url url)) site))

(define-fun url-site-schemeful ((url String) (site String)) Bool
  (let ((u (parse-url url)))
    (= (str.++ (u-proto u) "//" (u-site u)) site)))

(define-fun url-proto ((url String) (proto String)) Bool
    (= (u-proto (parse-url url)) proto))

(define-fun url-port ((url String) (port String)) Bool
    (= (u-port (parse-url url)) port))

(define-fun url-path ((url String) (path String)) Bool
    (= (u-path (parse-url url)) path))

(define-fun url-origin ((url String) (origin String)) Bool
    (= (u-origin (parse-url url)) origin))

(define-fun url-path-no-params ((url String) (path String)) Bool
    (= (u-path (parse-url url)) path))

(define-fun is-path-stronger ((candidate String) (current String) (original String)) Bool
  (and (is-prefix candidate original) (not (is-prefix candidate current))))

(define-fun-rec is-effective-cookie ((n Int) (tr (List Action)) (name String) (val String) (dom String) (path String) (cval String)) Bool
  (ite (< n 0) (= val cval)
    (match tr
      ((nil (= val cval))
      ((insert hd tl)
        (ite ((_ is cookie-jar-set) hd)
            (let  ((p     (ca-path (cjs-attrs hd)))
                   (d     (ca-domain (cjs-attrs hd)))
                   (nm    (cjs-name hd))
                   (v     (cjs-value hd)))
                  (ite (and (= name nm) (= d dom) (= p path))
                    (is-effective-cookie (- n 1) tl name val dom path v)
                    (is-effective-cookie (- n 1) tl name val dom path cval)))
            (is-effective-cookie (- n 1) tl name val dom path cval)))))))

(define-fun is-same-content-requested ((url1 String) (url2 String)) Bool
  (exists ((proto1 String)(proto2 String)(content String))
    (and (= url1 (str.++ proto1 "://" content))
         (= url2 (str.++ proto2 "://" content))
         (not (= proto1 proto2)))))

;Takes the ancestor list having the parent as first element and top level as last element and returns the origin of a data uri, ignoring elements having data uri themselves
(define-fun-rec get-data-URI-origin ((ancestors (List String))) String
  (match ancestors
    ((nil "")
     ((insert head tail) (ite (str.prefixof "data:" head) (get-data-URI-origin tail) head))))) ;ignore elements with data: uri

(define-fun-rec ancestors-permit-mixed-content ((ancestors (List String))) Bool
  (match ancestors
    ((nil true)
      ((insert a acs)
        (and
          (not (str.in.re a
            (re.++
              (str.to.re "https://")
              (re.* re.allchar))))
          (ancestors-permit-mixed-content acs))))))

; Checks if origin is potentially trustworthy according to W3C spec: https://www.w3.org/TR/secure-contexts/#potentially-trustworthy-origin
; Chromium implementation: https://source.chromium.org/chromium/chromium/src/+/refs/heads/main:services/network/public/cpp/is_potentially_trustworthy.cc;l=287
(define-fun is-origin-potentially-trustworthy ((URI String)) Bool
  ;Regexp: "^((https|wss):\/\/)|^(file:)|(:\/\/)(localhost|127(?:\.[0-9]+){0,2}\.[0-9]+)(:[0-9]{1,5})?((\/[0-9a-zA-Z%]+)+)?$|^about:blank$|^about:srcdoc$"
  (str.in.re URI
    (re.union
      (re.++  ;1a. URL's protocol is https
        (str.to.re "https://")
        (re.* re.allchar))
      (re.++  ;1b. URL's protocol is wss
        (str.to.re "wss://")
        (re.* re.allchar))
      (re.++  ;2. URL's protocol is file. Considered potentially trustworthy in Chromium: https://source.chromium.org/chromium/chromium/src/+/main:services/network/public/cpp/is_potentially_trustworthy.cc;l=316
        (str.to.re "file://")
        (re.* re.allchar))
      (re.++  ;3. URL's host is localhost as a domain (*.localhost or localhost)  Chromium implementation: https://source.chromium.org/chromium/chromium/src/+/main:net/base/url_util.cc;l=518
        (re.* (re.range "a" "z"))    ;protocol
        (str.to.re "://")
        (re.opt (re.* (re.++ (re.* (re.range "a" "z")) (str.to.re "."))))   ; subdomains
        (str.to.re "localhost")
        (re.opt (re.++ (str.to.re ":")((_ re.loop 1 5)(re.range "0" "9")))) ;optional port
        (str.to.re "/") ;slash to delimit path. Required as an exploit could be to have the previous part to be a subdomain
        (re.opt (re.* re.allchar))) ; optional path
      ;Loopback addresses are considered secure.
      ;There is a W3C community group report that proposes to implement stricter policies with regards to local network access: https://wicg.github.io/local-network-access/#mixed-content
      ;Thay propose to block mixed content served via loopback adresses when the content has been loaded from a public page.
      ;Another proposal is about a new CSP header that servers exposed on local networks can return: treat-as-public-address https://wicg.github.io/local-network-access/#csp
      ;When the header is present in the response, UAs should block mixed content even if loaded from loopback address.
      ;This proposals are not implemented by vendors
      (re.++  ;4a. URL's host is a loopback IPv4 address      Chromium implementation: https://source.chromium.org/chromium/chromium/src/+/refs/heads/main:net/base/ip_address.cc;l=290
        (re.* (re.range "a" "z"))              ;protocol
        (str.to.re "://127")                   ;start of loopback address 127.
        ((_ re.loop 1 3) (re.++ (str.to.re ".")(re.opt ((_ re.loop 1 3)(re.range "0" "9"))))) ; remaining of loopback address as concatenation of 3 digits separated by dots (3 times)
        (re.opt (re.++ (str.to.re ":")((_ re.loop 1 5)(re.range "0" "9")))) ;optional port
        (str.to.re "/")           ;slash to delimit path. Required as an exploit could be to have the previous part to be a subdomain
        (re.opt (re.* re.allchar))) ; optional path
      (re.++ ;4b. URL's host is a loopback IPv6 address    Chromium implementation: https://source.chromium.org/chromium/chromium/src/+/refs/heads/main:net/base/ip_address.cc;l=294
        (re.* (re.range "a" "z"))           ; protocol
        (str.to.re "://[")                  ; protocol-url delimiter and start of ipv6 address
        (re.opt                             ; optional multiple '0000..00:'
          (re.*
            (re.++
              (re.* (str.to.re "0"))        ; as many '0' as needed
              (str.to.re ":"))))               ; exactly one ':'
        (re.* (str.to.re ":")) ; multiple ':'
        (re.opt (re.* (str.to.re "0")))  ; zero or multiple '0'
        (str.to.re "1")   ; exactly one '1'
        (str.to.re "]") ; closing square bracket
        (re.opt (re.++ (str.to.re ":")((_ re.loop 1 5)(re.range "0" "9")))) ;optional port
        (re.opt (re.++ (str.to.re "/") (re.* re.allchar))))))) ; optional path delimiter and path

; Checks if url is potentially trustworthy according to W3C spec: https://w3c.github.io/webappsec-secure-contexts/#potentially-trustworthy-url
; Chromium implementation: https://source.chromium.org/chromium/chromium/src/+/refs/heads/main:services/network/public/cpp/is_potentially_trustworthy.cc;l=335
(define-fun is-url-potentially-trustworthy ((URI String)) Bool
  (or
    (str.in.re URI
      (re.union
        (str.to.re "about:blank")       ;1a. URL equals "about:blank"
        (str.to.re "about:srcdoc")      ;1b. URL equals "about:srcdoc"
        (re.++                          ;2. URL's protocol is data. In mixed-conent level2 such request is called a priori authenticated, see: https://w3c.github.io/webappsec-mixed-content/level2.html#terms
          (str.to.re "data:")
          (re.* re.allchar))))
    (is-origin-potentially-trustworthy URI)))

; data: URI do inherit the trustworthiness of its container page.
; If a data URI is the origin of a request we need to check its parent to understand if it is potentially trustworthy.
; Chromium testing data uri:  https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/loader/mixed_content_checker_test.cc;l=49
(define-fun is-data-origin-potentially-trustworthy ((URI String) (ancestors (List String))) Bool
  (and
    (url-proto URI "data:")
    (not (= nil ancestors))
    (is-origin-potentially-trustworthy (get-data-URI-origin (rev ancestors))))) ; look at the parent page to reason about trustworthiness

; Checks if should prohibit mixed security contexts. Excludes full ancestors analysis as it is performed by separate invariant.
(define-fun does-settings-prohibits-mixed-security-contexts-old ((origin-url String) (document-url-opt (Option String)) (ancestors (List String))) Bool
  (match document-url-opt
          (((some doc-url)
            (or
              (is-origin-potentially-trustworthy doc-url)
              (is-data-origin-potentially-trustworthy doc-url ancestors)))
          (none
            (or
              (is-origin-potentially-trustworthy origin-url)
              (is-data-origin-potentially-trustworthy origin-url ancestors))))))


; https://www.w3.org/TR/mixed-content/#categorize-settings-object
(define-fun does-settings-prohibits-mixed-security-contexts ((origin-url String) (document-url-opt (Option String)) (ancestors (List String))) Bool
  ; true ->  "Prohibits Mixed Security Contexts"; false -> "Does Not Restrict Mixed Security Contexts"
  (or
   ; 1. If settings’ origin is a potentially trustworthy origin, then return "Prohibits Mixed Security Contexts".
   (match document-url-opt
          (((some doc-url) (or (is-origin-potentially-trustworthy doc-url) (is-data-origin-potentially-trustworthy doc-url ancestors)))
           (none (or (is-origin-potentially-trustworthy origin-url) (is-data-origin-potentially-trustworthy origin-url ancestors)))))
   ; 2. If settings’ global object is a window, then: (1) Set document to settings’ global object's associated Document;
   ; 2.1 If navigable’s active document's origin is a potentially trustworthy origin, then return "Prohibits Mixed Security Contexts".
   (any! [] (ancestors a String) (is-origin-potentially-trustworthy a)))) ; if it is a window, ancestors could be non-empty otherwise we know that it is empty


; blob: URI inherits the trustworthiness of the context in which it was generated.
; This infromation is stored in the URI (present after the blob: scheme).
; Whatwg spec: https://url.spec.whatwg.org/#origin
; Chromium implementation: https://source.chromium.org/chromium/chromium/src/+/refs/heads/main:url/origin.cc;l=43;drc=b73134cfcce34a13f25202d077c0aa9dc03b662e;l=43
; We need to look at that to reason about trustworthiness
; If desired it is possible to implement the same thing for filesystem uri:  https://source.chromium.org/chromium/chromium/src/+/refs/heads/main:services/network/public/cpp/is_potentially_trustworthy.cc;l=350
(define-fun is-blob-url-potentially-trustworthy ((URI String)) Bool
  (and
    ;; (is-prefix "blob:" URI) ;URL's protocol is blob
    (url-proto URI "blob:")
    (is-url-potentially-trustworthy (u-path (parse-url URI))))) ; Reason about trustworthiness on substring starting after scheme

;Check if a resource type is about Upgradeable content (= passive mixed content) according to the W3C spec: https://www.w3.org/TR/mixed-content/#category-upgradeable
(define-fun is-mixed-content-upgradeable  ((type ResourceType)) Bool
  (or
     (= type type-image)
     (= type type-media)))
   ; (= type type-imageset) For historical reasons, imageset are not considered upgradable mixed content: https://github.com/w3c/webappsec-mixed-content/issues/64

; https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis-10#name-domain-matching
(define-fun domain-match ((cookie-domain String) (request-domain String)) Bool
 (or (= cookie-domain request-domain)
     (exists ((R String))
      (ite (= "." (str.at cookie-domain 0))
           (= request-domain (str.++ R cookie-domain))
           (= request-domain (str.++ R "." cookie-domain))))))

; https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-rfc6265bis-10#section-5.1.4
(define-fun path-match ((cookie-path  String) (request-path String)) Bool
  (or
   (= cookie-path request-path)
   (= cookie-path "/")
   (exists ((R String))
           (or
            (= (str.++ cookie-path "/") (str.++ request-path R))
            (= (str.++ cookie-path "/" R )  request-path)))))

(define-fun cookie-match ((path String) (domain String) (secure Bool) (url String)) Bool
  (and (domain-match domain (u-hostname (parse-url url)))
       (path-match path  (u-path (parse-url url)))
       (=> secure (is-origin-potentially-trustworthy url))))

(define-fun cookie-match-samesite ((same-site SameSite) (type ResourceType) (origin-url String) (method RequestMethod) (redir (List String)) (url String)) Bool
  (or
   (and (all! [(origin-url String)] (redir k String) (url-site k (u-site (parse-url origin-url))))
        (url-site url (u-site (parse-url origin-url))))
   (match same-site
          ((SS-None true)
           (SS-Lax (and (= type-main_frame type) (not (= M-POST method))))
           (SS-Strict (and (= type-main_frame type)
                           (= origin-url "")
                           (not (= M-POST method))))))))

(define-fun cookie-should-be-sent ((path String) (domain String) (secure Bool) (same-site SameSite) (type ResourceType) (origin-url String) (url String) (method RequestMethod) (redir (List String))) Bool
  (and (cookie-match path domain secure url)
       (cookie-match-samesite same-site type origin-url method redir url)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invariants -- Cookies


; WebSpec Table I (I.1)
(define-fun secure-cookies-invariant ((tr (List Action))) Bool
  (forall ((t1 Int) (t2 Int) (url String) (set-cookie-headers (List String)) (set-cookie String) (body (Option String)) (R String) (R1 String) (name String) (value String) (http-only Bool) (same-site SameSite) (path String) (domain String) (id String) (ctx Ctx) (secure Bool))
    (=> (and
         (> t2 t1)
         (or
            (and (at t1 tr (net-response id url (response-headers set-cookie-headers) body))
              (in set-cookie-headers set-cookie)
              (in (split-cookie set-cookie) (str.++ name "=" value))
              (at t2 tr (cookie-jar-set name value (cookie-attrs true http-only same-site path domain)))
              (in (split-cookie set-cookie) "Secure"))
            (and (at t1 tr (js-set-cookie ctx set-cookie R1))
              (= url (ctx-location ctx))
              (in (split-cookie set-cookie) (str.++ name "=" value))
              (in (split-cookie set-cookie) "Secure")
              ; more precise
              (at t2 tr (cookie-jar-set name value (cookie-attrs secure http-only same-site path domain)))
              (= true secure))))
        (or (is-prefix "wss" url) (is-prefix "https" url)))))

; WebSpec Table I (I.2)
(define-fun http-only-invariant ((tr (List Action))) Bool
 (forall ((t0 Int) (t1 Int) (ctx Ctx) (name String) (value String) (secure Bool) (http-only Bool) (same-site SameSite) (path String) (cpath String) (domain String) (cookies String))
   (=> (and
         (> t1 t0)
         (at t0 tr (cookie-jar-set name value (cookie-attrs secure http-only same-site cpath domain)))
         (at t1 tr (js-get-cookie ctx cookies))
         (in (split-cookie cookies) (str.++ name "=" value))
         (cookie-match path domain secure (ctx-location ctx)))
       (= http-only false))))

; WebSpec Table I (I.3)
(define-fun host-invariant ((tr (List Action))) Bool
 (forall ((t1 Int) (t2 Int) (ctx Ctx) (url String) (body (Option String)) (host String) (port String) (cname String) (cvalue String) (P String) (R String) (R1 String) (set-cookie-headers (List String)) (set-cookie String) (secure Bool) (http-only Bool) (same-site SameSite) (path String) (domain String) (id String))
   (=>
    (and
     (> t2 t1)
     (or
      (and
       (at t1 tr (net-response id url (response-headers set-cookie-headers) body))
       (in set-cookie-headers set-cookie)
       (in (split-cookie set-cookie) (str.++ "__Host-" cname "=" cvalue))
       (url-domain url host)
       (url-port url port))
      (and
       (at t1 tr (js-set-cookie ctx set-cookie R1))
       (in (split-cookie set-cookie) (str.++ "__Host-" cname "=" cvalue) )
       (url-domain (ctx-location ctx) host)
       (url-port (ctx-location ctx) port)))
     (at t2 tr (cookie-jar-set (str.++ "__Host-" cname) cvalue (cookie-attrs secure http-only same-site path domain))))
     (or (= domain host)
         (= host (str.++ domain ":" port))))))

;; Site cookies can be set only by toplevel navigations and same-site subframes and ...
(define-fun samesite-cookies-integrity ((tr (List Action))) Bool
 (forall ((t1 Int) (t2 Int) (t3 Int) (method RequestMethod) (type ResourceType) (origin-url String) (document-url (Option String)) (frame-ancestors (List String)) (redirs (List String)) (headers RequestHeaders) (body (Option String)) (rbody (Option String)) (cname String) (cvalue String) (secure Bool) (http-only Bool) (same-site SameSite) (SS String) (path String) (domain String) (url String) (site String) (id String) (set-cookie-headers (List String)) (set-cookie String))
   (=>
    (and
     (> t2 t1) (> t3 t1)
     (at t1 tr (net-request id url method type origin-url document-url frame-ancestors redirs headers body))
     (at t2 tr (net-response id url (response-headers set-cookie-headers) rbody))
     (in set-cookie-headers set-cookie)
     (in (split-cookie set-cookie) (str.++ cname "=" cvalue))
     (in (split-cookie set-cookie) (str.++ "SameSite=" SS))
     (or (and (= SS "Lax") (= same-site SS-Lax))
         (and (= SS "Strict") (= same-site SS-Strict)))
     (at t3 tr (cookie-jar-set cname cvalue (cookie-attrs secure http-only same-site path domain)))
     (path-match path (u-path (parse-url url)))
     (domain-match domain (u-hostname (parse-url url)))
     (url-site url site))
    (or
     (= type type-main_frame)
     (url-site origin-url site)))))

(define-fun secure-cookies-confidentiality ((tr (List Action))) Bool
  (forall ((t0 Int) (t1 Int) (url String) (body (Option String))
           (name String) (value String) (http-only Bool) (same-site SameSite) (path String) (domain String) (url-req String) (hd-origin (Option String)) (ctx Ctx)
           (origin-url String) (frame-ancestors (List String)) (redirs (List String)) (id String) (cookies String) (method RequestMethod) (type ResourceType) (document-url (Option String)) (req-path String))
    (=>
      (and
       (> t1 t0)
       (at t0 tr (cookie-jar-set name value (cookie-attrs true http-only same-site path domain))) ;; there is a set cookie with secure=true
       (or
        (and
         (at t1 tr (net-request id url method type origin-url document-url frame-ancestors redirs (request-headers (some cookies) hd-origin) body))
         (cookie-should-be-sent path domain false same-site type origin-url url method redirs)) ;; cookie should be sent, ignoring the secure attribute
        (and  (at t1 tr (js-get-cookie ctx cookies))
              (= url (ctx-location ctx))
              (cookie-match path domain false url)))
       (in (split-cookie cookies) (str.++ name "=" value))
       (is-effective-cookie t1 tr name value domain path ""))
      (is-origin-potentially-trustworthy url))))

(define-fun samesite-cookies-confidentiality ((tr (List Action))) Bool
 (forall ((t1 Int) (t2 Int) (t3 Int) (method RequestMethod) (type ResourceType) (origin-url String) (document-url (Option String)) (frame-ancestors (List String)) (redirs (List String)) (cookies String) (body (Option String)) (cname String) (cvalue String) (secure Bool) (http-only Bool) (same-site SameSite) (path String) (domain String) (url String) (req-domain String) (req-path String) (hd-origin (Option String)) (id String) (ctx Ctx) (cookies-p String) (id-p String) (url-p String) (method-p RequestMethod) (type-p ResourceType) (origin-url-p String) (document-url-p (Option String)) (frame-ancestors-p (List String)) (redirs-p (List String)) (body-p (Option String))  (hd-origin-p (Option String)))
  (=>
   (and
    (> t2 t1) (> t3 t2)
    (at t1 tr (cookie-jar-set cname cvalue (cookie-attrs secure http-only same-site path domain)))
    (or (= same-site SS-Lax) (= same-site SS-Strict))

    (at t2 tr (net-request id url method type origin-url document-url frame-ancestors redirs
                           (request-headers (some cookies) hd-origin) body))
    (cookie-match path domain secure url)
    (not (cookie-match-samesite same-site type origin-url method redirs url))
    (or
     (and
      (at t3 tr (js-get-cookie ctx cookies-p))
      (= url (ctx-location ctx)))
     (and
      (at t3 tr (net-request id-p url-p method-p type-p origin-url-p document-url-p frame-ancestors-p redirs-p (request-headers (some cookies-p) hd-origin-p) body-p))
      (= document-url-p (some url))
      (cookie-should-be-sent path domain secure same-site type-p origin-url-p url-p method-p redirs-p))))
   (and
    (not (in (split-cookie cookies) (str.++ cname "=" cvalue)))
    (not (in (split-cookie cookies-p) (str.++ cname "=" cvalue)))))))


(define-fun cookie-serialization-invariant ((tr (List Action))) Bool
 (forall ((t1 Int) (t2 Int) (method RequestMethod) (type ResourceType) (origin-url String) (document-url (Option String)) (frame-ancestors (List String)) (redirs (List String)) (cookies String) (body (Option String)) (cname String) (cvalue String) (secure Bool) (http-only Bool) (same-site SameSite) (path String) (domain String) (url String) (req-domain String) (req-path String) (hd-origin (Option String)) (id String) (ctx Ctx))
    (=>
     (and
      (> t2 t1)
      (at t1 tr (cookie-jar-set cname cvalue (cookie-attrs secure http-only same-site path domain)))
      (or
       (and
        (at t2 tr (net-request id url method type origin-url document-url frame-ancestors redirs (request-headers (some cookies) hd-origin) body))
        (cookie-should-be-sent path domain secure same-site type origin-url url method redirs))
       (and (at t2 tr (js-get-cookie ctx cookies))
              (= url (ctx-location ctx))
              (cookie-match path domain secure url)))
      (is-effective-cookie t2 tr cname cvalue domain path ""))
     (in (split-cookie cookies) (str.++ cname "=" cvalue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invariants -- Mixed Content

; https://www.w3.org/TR/mixed-content/#should-block-fetch and https://www.w3.org/TR/mixed-content/#should-block-response
(define-fun blockable-mixed-content-filtered ((tr (List Action))) Bool
  (forall ((t1 Int) (url String) (method RequestMethod) (type ResourceType) (origin-url String) (document-url (Option String)) (frame-ancestors (List String)) (redirs (List String)) (request-headers RequestHeaders) (request-body (Option String)) (id String))
    (=>
     (at t1 tr (net-request id url method type origin-url document-url frame-ancestors redirs request-headers request-body))
     ; return allowed if one or more of the following conditions are met:
     (or
      ; 1. § 4.3 Does settings prohibit mixed security contexts? returns "Does Not Restrict Mixed Security Contexts" when applied to request’s client.
      (not (does-settings-prohibits-mixed-security-contexts origin-url document-url frame-ancestors))
      ; 2.  request’s URL is a potentially trustworthy URL.
      (is-url-potentially-trustworthy url)
      ; 4. request’s destination is "document", and request’s target browsing context has no parent browsing context.
      (and (= type type-main_frame) (= nil frame-ancestors))))))

(define-fun upgradeable-mixed-content-filtered ((tr (List Action))) Bool
  (forall ((t1 Int) (url String) (method RequestMethod) (type ResourceType) (origin-url String) (document-url (Option String)) (frame-ancestors (List String)) (redirs (List String)) (request-headers RequestHeaders) (request-body (Option String)) (id String))
    (=>
     (and
      (at t1 tr (net-request id url method type origin-url document-url frame-ancestors redirs request-headers request-body))
      (not (is-url-potentially-trustworthy url))
      (not (= type type-main_frame)))
     ; If one or more of the following conditions is met, return without modifying request:
     (and
      ; 3. § 4.3 Does settings prohibit mixed security contexts? returns "Does Not Restrict Mixed Security Contents" when applied to request’s client.
      (not (does-settings-prohibits-mixed-security-contexts origin-url document-url frame-ancestors))
      ; 4. request’s destination is not "image", "audio", or "video".
      (not (is-mixed-content-upgradeable type))))))
