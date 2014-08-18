(ns snake.work
  (:require [snake.core :refer (run-not-grow run-grow run-many-apples run-with-walls neib-cell dirs)]))

;;; You're writing a bot for playing snake.
;;; So, you are a snake and your goal is to collect apples.
;;; Field size: 40 x 30
;;; Every turn you move to one of the adjacent cell.
;;; Your function must take 2 arguments: snake position and apple position and decide which direction to move.
;;; Directions are: :up, :down, :left, :right (they are keywords). Your function must return one of these directions.
;;; Position (snake or apple) is a vector of 2 elements: x and y.
;;; In this task snake is not growing from eating apples so there is no danger of snake hitting itself.
;;; Note: upper left corner cell is (0, 0).

(defn not-grow-new-direction [[sx sy :as snake] [ax ay :as apple]]
  (cond
    (< sx ax) :right
    (> sx ax) :left
    (< sy ay) :down
    (> sy ay) :up
    :else :right))

(run-not-grow not-grow-new-direction)



;;; Snake is growing now. (each time snake eats an apple, body length increases).
;;; You need to write similar function as in previous task.
;;; It takes 2 arguments.
;;; First argument is snake body - collection of cells, each cell is a vector of x and y. First cell is the head.
;;; Second argument is apple position - vector of x and y.
;;; It should return direction: :up, :down, :left or :right.
;;; Note that you cannot change direction to the opposite in 1 move: snake will hit it's tail if length is 2 or more.
;;; Well, you can change direction but snake will die :\


(defn safe? [dir head dangers]
  (let [next-head (neib-cell head dir)]
    (not (some #{next-head} dangers))))

(defn growing-new-direction [[[hx hy :as head] & rst] [ax ay :as apple]]
  (cond
   (and (safe? :right head rst) (< hx ax)) :right
   (and (safe? :left head rst) (> hx ax)) :left
   (and (safe? :down head rst) (< hy ay)) :down
   (and (safe? :up head rst) (> hy ay)) :up
   (safe? :right head rst) :right
   (safe? :left head rst) :left
   (safe? :down head rst) :down
   (safe? :up head rst) :up
   :else :right))

(run-grow growing-new-direction)



;;; Now you have many apples (five) instead of one.
;;; Function the same as previous but it takes set of apples instead of the single apple.
;;; Each apple in the set is a vector of x and y.

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn closest [point points]
  (apply min-key (partial distance point) points))

(defn many-apples-new-direction [[head & _ :as snake] apples]
  (growing-new-direction snake (closest head apples)))

(run-many-apples many-apples-new-direction)



;;; Walls are added. So snake can hit wall and die.
;;; Your function now takes third argument - set of walls.
;;; Each wall is a cell that snake is not allowed to bump to.
;;; Wall is a vector of x and y.

(defn walls-new-direction [snake apples walls]
  (many-apples-new-direction (concat snake walls) apples))
(run-with-walls walls-new-direction)
