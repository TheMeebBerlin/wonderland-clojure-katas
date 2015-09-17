(ns alphabet-cipher.coder)

(defn char-to-int [character]
  (- (int character) 97))

(defn get-row [index]
  (->>  (cycle "abcdefghijklmnopqrstuvwxyz") 
    ; let's say index is 5
    ; we drop the first 5 elements and cycle from "f"
    (drop index)
    ; we take 26 elements from the cycle
    (take 26)
    ; and we put them into a vector
    (into [])))

(defn get-mapped-row [index]
  (let [row (get-row index)] 
    ; reduce with apply fn individually of whatever map-indexed
    ; gives us and put result in the map {}
    (reduce 
          ; acc is {} 
                ; [i c] is e.g. [e 2]
                      ; assoc does the mapping 
                      ; e.g. { :e 2, :f 3, .. }
      (fn [acc [i c]] (assoc acc c i)) 

      {} 
      ; returns e.g. {c 0 d 1 e 2 ... b 25}
      (map-indexed vector row))))

(defn encode-char [k m]
  (let [ki (char-to-int k) 
        mi (char-to-int m)
        row (get-row ki)]
    (nth row mi)))

(defn decode-char [k c]
  (let [ki (char-to-int k)
        mapped-row (get-mapped-row ki)]
    ; convert integer 98 to character \b
          ; convert the alphabet position to the unicode position i.e. b as 1 = 98
                ; get key c's corresponding value from mapped-row
                ; i.e. mapped-row[c] in ruby
    (char (+ 97 (get mapped-row c)))))

(defn encode [keyword message]
              ; using map with cycle: we will cycle through keyword
              ; until we reach end of message
  (apply str (map encode-char (cycle keyword) message)))

(defn decode [keyword message]
  (apply str (map decode-char (cycle keyword) message)))


