;; This project was auto formatted by cljfmt
(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.set :as set])
  (:gen-class))

(def the-map
{
    :begin {:desc "This is the first day of the fall semester, you are going back to school."
           :title "the first day."
           :commands "class, study, home"
           :dir {:class :lecture
                 :study :library
                 :home  :apartment}
           :contents #{}}

     :lecture {:desc "You go to the CS225 lecture."
             :title "in the class."
             :commands "sleep, listen."
             :dir {:listen :afterclass}
             :contents #{:angry}}

     :afterclass {:desc "After class, the professor is about leaving."
                :title "after class."
                :commands "talk, leave"
                :dir {:leave :begin}
                :contents #{:hint :partner}}

      :library {:desc "You go to the library."
             :title "in the library."
             :commands "explore, leave."
             :dir {:leave :begin}
             :contents #{:book}}


     :apartment {:desc "You go back to your apartment."
               :title "in your apartment."
               :commands "play, relax, leave"
               :dir {:relax :one_week
                     :leave :begin}
               :contents #{:game}}

   :one_week {:desc "One weeks later, you receives your first mp for CS225."
              :title "one week later."
              :commands "solve, ignore."
              :dir {:ignore :fail}
              :contents #{:exp}}

  :finish_mp {:desc "You finished the mp and then the midterm is comming soon."
               :title "after you finish your mp."
               :commands "course, review, officehour, ignore."
               :dir {:ignore :miss_midterm}
               :contents #{:lecturenote}}

   :miss_midterm {:desc "You play games and stay up late before midterm and you miss your midterm."
                  :title "you miss your midterm."
                  :commands "ignore, argue, drop"
                  :dir {:ignore :fail
                        :argue :fail
                        :drop :fail}
                  :contents #{}}

   :pass_midterm {:desc "Finally you passed the midterm."
                  :title "after midterm."
                  :commands "relax, I_love_study"
                  :dir {:relax :street}
                  :contents #{:badmood}}

   :street {:desc "You decide to relax after exam. Where to go?"
              :title "on the street."
              :commands "bar, lib"
              :dir {:lib :final}
              :contents #{:hurt}}

   :final {:desc "It is the final week, you are going to prepare for the final exam."
           :title "the final week."
           :commands "soeasy, hangout, lib"
           :dir {:soeasy :fail
                 :lib :in_exam}
           :contents #{:luck}}

  :in_exam {:desc "During the final exam, you find the question is superhard!"
              :title "during the exam."
              :commands "cheat, thinking, leave"
              :dir {:cheat :fail
                    :leave :fail}
              :contents #{}}

   :in_exam2 {:desc "Even though you solved most of the questions but you still think some of
              the problems has no answes."
              :title "still in the exam."
              :commands "cheat, blank, guess"
              :dir {:cheat :fail
                    :blank :happy_end
                    :guess :fail}
              :contents #{}}

   :happy_end {:desc "You leave the answer blank. But finally, the professor says that they make
               some mistakes for the exam questions and gives every one who left blank answers
               full scores! Finally you passed CS225 and get an A+! Commands: exit"
               :title "after final exam"
               :commands "exit"
               :dir {}
               :contents #{}}

   :fail {:desc "You are not well_prepared for CS225 and you drop the course.
          Commands: exit"
          :title "a pity"
          :commands "exit"
          :dir {}
          :contents #{}}


})

(def adventurer
  {:location :begin
   :inventory #{:hint :partner :book :exp :lecture_note}
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "It is " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (print (str "Commands: " (-> the-map location :commands) ". "))
    (update-in player [:seen]
               #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn tock [player]
  (update-in player [:tick] inc))


(defn inven [player]
  (print "Currently you have ")
  (apply println (player :inventory)
  player))

(defn sleep [player]
 (do
      (println "You sleep during the class and the professor is pissed off")
      (update-in player [:inventory] #(conj % :angry))
))


(defn talk [player]
  (let [room (get-in player [:location]) inventory(player :inventory)]
    (let [item (the-map [room :contents])]
      (if  (contains? inventory :angry)
        (do(println "Because you offend the professor during class, he refused to talk with you.")player)

      (do
      (println "You talk with professor and he gives you some hint. Then you talk to your classmates and find a lab partner.")
      (update-in player [:inventory] #(conj % :partner :hint))
      )
      ))))

(defn explore [player]
 (do
      (println "You explore the library and find a text book.")
      (update-in player [:inventory] #(conj % :book))
))


(defn play [player]
        (do
          (println "Your roommate recommand a very interesting game for you and you are addict to it.")
          (update-in player [:inventory] #(conj % :game))))

(defn solve [player]
      (println "You try to solve the mp for a while and you gain some experience from it.")


  (let [room (get-in player [:location]) inventory(player :inventory)]
      (if (contains? inventory :partner)
        (do(println "With the help of your partner, you finish your mp.")
        (assoc-in player [:location] :finish_mp)
      )
        (do(println "The mp has a severe bug, you can't solve it.")
        (assoc-in player [:location] :fail))
      )))


(defn course [player]
   (do
      (println "You go to the lecture and wite down some lecture notes.")
      (update-in player [:inventory] #(conj % :lecture_note))
))

(defn officehour [player]
   (do
      (println "You go to the office hour and gain some experiences.")
      (update-in player [:inventory] #(conj % :exp))
))

(defn review [player]
  (let [room (get-in player [:location]) inventory(player :inventory)]
    (let [item (the-map [room :contents])]
        (do(println "You decide to do some reviews for midterm."));

  (if  (and (contains? inventory :hint)(contains? inventory :lecture_note)(contains? inventory :exp))
      (do
        (if  (contains? inventory :game)
          (do(println "But you also want to play games so you give it up. Finally, you get 0 for the midterm.")
          (assoc-in player [:location] :fail))
          (do
              (println "After you review the lecture notes, mps and remember that the professor has already given you some hints, you get 100 for the midterm.")
              (assoc-in player [:location] :pass_midterm)
          )
        )
      )

      (do(println "But you do not have enough materials.")player)

  )
)))

(defn I_love_study [player]
    (do
      (println "You still decide to study after midterm which makes you begin to hate study.")
      (update-in player [:inventory] #(conj % :badmood))))

(defn bar [player]
    (do
      (println "You decide to go to the bar, but unluckly, a drunkard punch on your head for several times and you have to go back home.")
      (update-in player [:inventory] #(conj % :hurt))))

(defn hangout [player]
  (let [room (get-in player [:location]) inventory(player :inventory)]
    (let [item (the-map [room :contents])]
    (do(println "You decide to hang out."));

  (if  (contains? inventory :badmood)
      (do(println "You suddenly feel sad with no reason so that you go back to home.")player)
      (do
        (if  (contains? inventory :book)
          (do(println "You meet a friend on the street and he reminds you that the text book is really helpful for the final exam.")
          (update-in player [:inventory] #(conj % :luck)))
          (do
              (println "You feel boring and you go back to home.")player
          )
        )
      )

  )
)))

(defn thinking [player]
  (let [room (get-in player [:location]) inventory(player :inventory)]
    (let [item (the-map [room :contents])]
    (do(println "You continue thinking about the questions."));

  (if  (contains? inventory :hurt)
      (do(println "Since your head was hit by drunkard, you cannot concentrate on your exam.")player)
      (do
        (if  (contains? inventory :luck)
          (do(println "You remembered the book reviewed before the exam and solved all the questions.")
          (assoc-in player [:location] :in_exam2))
          (do
              (println "You continue thinking for a while but still cannot solve the problem.")player
          )
        )
      )

  )
)))

(defn exit [player]
  (do (println "You reach the end of the game.")player)
  (System/exit 0)
)

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)
         [:inventory] (inven player)
         [:sleep] (sleep player)
         [:talk] (talk player)
         [:explore] (explore player)
         [:play] (play player)
         [:solve] (solve player)
         [:course] (course player)
         [:review] (review player)
         [:officehour] (officehour player)
         [:I_love_study] (I_love_study player)
         [:bar] (bar player)
         [:hangout] (hangout player)
         [:thinking] (thinking player)
         [:exit] (exit player)


         [:class] (go :class player)
         [:study] (go :study player)
         [:home] (go :home player)
         [:listen] (go :listen player)
         [:leave] (go :leave player)
         [:relax] (go :relax player)
         [:ignore] (go :ignore player)
         [:ignore] (go :ignore player)
         [:argue] (go :argue player)
         [:drop] (go :drop player)
         [:soeasy] (go :soeasy player)
         [:lib] (go :lib player)
         [:cheat] (go :cheat player)
         [:blank] (go :blank player)
         [:guess] (go :guess player)

    _ (do (println "Invalid instruction. Please try again.")
          player)))

(defn -main
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
