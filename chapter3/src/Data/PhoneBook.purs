module Data.PhoneBook where

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry = { firstName :: String, lastName :: String, phone :: String }

type PhoneBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ entry.phone

emptyBook :: PhoneBook
emptyBook = empty

insertEntry :: Entry -> PhoneBook -> PhoneBook
insertEntry = Cons
 
findEntry :: String -> String -> PhoneBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByPhone :: String -> PhoneBook -> Maybe Entry
findEntryByPhone phone = head <<< filter (\e -> e.phone == phone)

nameOccurs :: String -> String -> PhoneBook -> Boolean
nameOccurs firstName lastName = null <<< filter filterEntry 
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

nameOccurs' :: String -> String -> PhoneBook -> Boolean
nameOccurs' first last book = case findEntry first last book of
                Nothing -> false
                Just e  -> true

removeDuplicates :: PhoneBook -> PhoneBook
removeDuplicates = nubBy (\e1 e2 -> e1.firstName == e2.firstName && e1.lastName == e2.lastName)
