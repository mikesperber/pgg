(define ($goal-35-537 clone-538)
  (let ((var-540 (stream-car clone-538)))
    (let ((var-541 (car var-540)))
      (loop_5-22-119-542 var-541
                         var-540
                         '()
                         clone-538))))
(define (loop_5-22-119-542 clone-546 clone-545 clone-544 clone-543)
  (if (equal? 'n clone-546)
      (let ((var-548 (stream-cdr clone-543)))
        (let ((var-550 (stream-car var-548)))
          (let ((var-590 (inner-loop_1-28-484-551 var-550
                                                  var-548
                                                  (cons (cdr clone-545)
                                                        clone-544)
                                                  var-548)))
            (let ((var-591 (result-dot var-590)))
              (parse-bar-18-172-592 var-591
                                    (result-lhs var-590)
                                    (result-att var-590)
                                    clone-544
                                    (result-inp var-590))))))
      (loop_5-22-119-1701 clone-546 clone-545 clone-544 clone-543)))
(define (loop_5-22-119-1701 clone-1705 clone-1704 clone-1703 clone-1702)
  (if (equal? 'l clone-1705)
      (let ((var-1707 (stream-cdr clone-1702)))
        (let ((var-1708 (stream-car var-1707)))
          (let ((var-1709 (car var-1708)))
            (let ((var-1710 (loop_5-22-119-903 var-1709
                                               var-1708
                                               (cons (cdr clone-1704)
                                                     clone-1703)
                                               var-1707)))
              (let ((var-1711 (result-dot var-1710)))
                (parse-bar-18-172-592 var-1711
                                      (result-lhs var-1710)
                                      (result-att var-1710)
                                      clone-1703
                                      (result-inp var-1710)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1705)))
(define (parse-bar-18-172-592 clone-597 clone-596 clone-595 clone-594 clone-593)
  (if (< 1 clone-597)
      (parse-result clone-596 (- clone-597 1) clone-595 clone-593)
      (loop_6-20-220-599 clone-596 clone-595 clone-594 clone-593)))
(define (loop_6-20-220-599 clone-603 clone-602 clone-601 clone-600)
  (if (equal? 'e clone-603)
      (let ((var-609 (direct-parse-13-57-606 (cons clone-602 clone-601)
                                             clone-600)))
        var-609)
      (loop_6-20-220-611 clone-603 clone-602 clone-601 clone-600)))
(define (loop_6-20-220-611 clone-615 clone-614 clone-613 clone-612)
  (if (equal? 't clone-615)
      (let ((var-619 (stream-car clone-612)))
        (let ((var-1691 (inner-loop_1-28-484-620 var-619
                                                 clone-612
                                                 (cons clone-614 clone-613)
                                                 clone-612)))
          (let ((var-1692 (result-dot var-1691)))
            (parse-bar-18-172-592 var-1692
                                  (result-lhs var-1691)
                                  (result-att var-1691)
                                  clone-613
                                  (result-inp var-1691)))))
      (let ((var-1697 (stream-car clone-612)))
        (let ((var-1698 (inner-loop_1-28-484-712 var-1697
                                                 clone-612
                                                 (cons clone-614 clone-613)
                                                 clone-612)))
          (let ((var-1699 (result-dot var-1698)))
            (parse-bar-18-172-592 var-1699
                                  (result-lhs var-1698)
                                  (result-att var-1698)
                                  clone-613
                                  (result-inp var-1698)))))))
(define (inner-loop_1-28-484-620 clone-624 clone-623 clone-622 clone-621)
  (let ((var-626 (equal? clone-624 '$)))
    (if (loop_4-30-500-627 var-626 clone-624)
        (parse-result 'e 1 ((lambda ($1) $1) (car clone-622)) clone-623)
        (let ((var-637 (stream-car clone-623)))
          (let ((var-638 (car var-637)))
            (loop_2-33-338-639 var-638 var-637 clone-622 clone-623))))))
(define (loop_2-33-338-639 clone-643 clone-642 clone-641 clone-640)
  (if (equal? '- clone-643)
      (let ((var-645 (stream-cdr clone-640)))
        (let ((var-646 (stream-car var-645)))
          (let ((var-647 (car var-646)))
            (let ((var-1597 (loop_5-22-119-648 var-647
                                               var-646
                                               (cons (cdr clone-642) clone-641)
                                               var-645)))
              (let ((var-1598 (result-dot var-1597)))
                (parse-result (result-lhs var-1597)
                              (- var-1598 1)
                              (result-att var-1597)
                              (result-inp var-1597)))))))
      (loop_2-33-338-1600 clone-643 clone-642 clone-641 clone-640)))
(define (loop_2-33-338-1600 clone-1604 clone-1603 clone-1602 clone-1601)
  (if (equal? '+ clone-1604)
      (let ((var-1606 (stream-cdr clone-1601)))
        (let ((var-1607 (stream-car var-1606)))
          (let ((var-1608 (car var-1607)))
            (let ((var-1688 (loop_5-22-119-1609 var-1608
                                                var-1607
                                                (cons (cdr clone-1603)
                                                      clone-1602)
                                                var-1606)))
              (let ((var-1689 (result-dot var-1688)))
                (parse-result (result-lhs var-1688)
                              (- var-1689 1)
                              (result-att var-1688)
                              (result-inp var-1688)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1604)))
(define (loop_5-22-119-1609 clone-1613 clone-1612 clone-1611 clone-1610)
  (if (equal? 'n clone-1613)
      (let ((var-1615 (stream-cdr clone-1610)))
        (let ((var-1617 (stream-car var-1615)))
          (let ((var-1618 (inner-loop_1-28-484-551 var-1617
                                                   var-1615
                                                   (cons (cdr clone-1612)
                                                         clone-1611)
                                                   var-1615)))
            (let ((var-1619 (result-dot var-1618)))
              (parse-bar-18-172-1620 var-1619
                                     (result-lhs var-1618)
                                     (result-att var-1618)
                                     clone-1611
                                     (result-inp var-1618))))))
      (loop_5-22-119-1676 clone-1613 clone-1612 clone-1611 clone-1610)))
(define (loop_5-22-119-1676 clone-1680 clone-1679 clone-1678 clone-1677)
  (if (equal? 'l clone-1680)
      (let ((var-1682 (stream-cdr clone-1677)))
        (let ((var-1683 (stream-car var-1682)))
          (let ((var-1684 (car var-1683)))
            (let ((var-1685 (loop_5-22-119-903 var-1684
                                               var-1683
                                               (cons (cdr clone-1679)
                                                     clone-1678)
                                               var-1682)))
              (let ((var-1686 (result-dot var-1685)))
                (parse-bar-18-172-1620 var-1686
                                       (result-lhs var-1685)
                                       (result-att var-1685)
                                       clone-1678
                                       (result-inp var-1685)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1680)))
(define (parse-bar-18-172-1620 clone-1625
                               clone-1624
                               clone-1623
                               clone-1622
                               clone-1621)
  (if (< 1 clone-1625)
      (parse-result clone-1624 (- clone-1625 1) clone-1623 clone-1621)
      (loop_6-20-220-1627 clone-1624 clone-1623 clone-1622 clone-1621)))
(define (loop_6-20-220-1627 clone-1631 clone-1630 clone-1629 clone-1628)
  (if (equal? 'e clone-1631)
      (let ((var-1635 (stream-car clone-1628)))
        (let ((var-1654 (inner-loop_1-28-484-1636 var-1635
                                                  clone-1628
                                                  (cons clone-1630 clone-1629)
                                                  clone-1628)))
          (let ((var-1655 (result-dot var-1654)))
            (parse-bar-18-172-1620 var-1655
                                   (result-lhs var-1654)
                                   (result-att var-1654)
                                   clone-1629
                                   (result-inp var-1654)))))
      (loop_6-20-220-1657 clone-1631 clone-1630 clone-1629 clone-1628)))
(define (loop_6-20-220-1657 clone-1661 clone-1660 clone-1659 clone-1658)
  (if (equal? 't clone-1661)
      (let ((var-1665 (stream-car clone-1658)))
        (let ((var-1666 (inner-loop_1-28-484-620 var-1665
                                                 clone-1658
                                                 (cons clone-1660 clone-1659)
                                                 clone-1658)))
          (let ((var-1667 (result-dot var-1666)))
            (parse-bar-18-172-1620 var-1667
                                   (result-lhs var-1666)
                                   (result-att var-1666)
                                   clone-1659
                                   (result-inp var-1666)))))
      (let ((var-1672 (stream-car clone-1658)))
        (let ((var-1673 (inner-loop_1-28-484-712 var-1672
                                                 clone-1658
                                                 (cons clone-1660 clone-1659)
                                                 clone-1658)))
          (let ((var-1674 (result-dot var-1673)))
            (parse-bar-18-172-1620 var-1674
                                   (result-lhs var-1673)
                                   (result-att var-1673)
                                   clone-1659
                                   (result-inp var-1673)))))))
(define (inner-loop_1-28-484-1636 clone-1640 clone-1639 clone-1638 clone-1637)
  (let ((var-1642 (equal? clone-1640 '$)))
    (if (loop_4-30-500-627 var-1642 clone-1640)
        (let ((var-1647 (cdr clone-1638)))
          (let ((var-1648 (cdr var-1647)))
            (parse-result 'e
                          3
                          ((lambda ($1 $2 $3) (+ $1 $3))
                           (car var-1648)
                           (car var-1647)
                           (car clone-1638))
                          clone-1639)))
        (let ((var-1652 (stream-car clone-1639)))
          (let ((var-1653 (car var-1652)))
            (_sim-error 'direct-parse "can't shift on" var-1653))))))
(define (loop_5-22-119-648 clone-652 clone-651 clone-650 clone-649)
  (if (equal? 'n clone-652)
      (let ((var-654 (stream-cdr clone-649)))
        (let ((var-656 (stream-car var-654)))
          (let ((var-657 (inner-loop_1-28-484-551 var-656
                                                  var-654
                                                  (cons (cdr clone-651)
                                                        clone-650)
                                                  var-654)))
            (let ((var-658 (result-dot var-657)))
              (parse-bar-18-172-659 var-658
                                    (result-lhs var-657)
                                    (result-att var-657)
                                    clone-650
                                    (result-inp var-657))))))
      (loop_5-22-119-1585 clone-652 clone-651 clone-650 clone-649)))
(define (loop_5-22-119-1585 clone-1589 clone-1588 clone-1587 clone-1586)
  (if (equal? 'l clone-1589)
      (let ((var-1591 (stream-cdr clone-1586)))
        (let ((var-1592 (stream-car var-1591)))
          (let ((var-1593 (car var-1592)))
            (let ((var-1594 (loop_5-22-119-903 var-1593
                                               var-1592
                                               (cons (cdr clone-1588)
                                                     clone-1587)
                                               var-1591)))
              (let ((var-1595 (result-dot var-1594)))
                (parse-bar-18-172-659 var-1595
                                      (result-lhs var-1594)
                                      (result-att var-1594)
                                      clone-1587
                                      (result-inp var-1594)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1589)))
(define (parse-bar-18-172-659 clone-664 clone-663 clone-662 clone-661 clone-660)
  (if (< 1 clone-664)
      (parse-result clone-663 (- clone-664 1) clone-662 clone-660)
      (loop_6-20-220-666 clone-663 clone-662 clone-661 clone-660)))
(define (loop_6-20-220-666 clone-670 clone-669 clone-668 clone-667)
  (if (equal? 'e clone-670)
      (let ((var-674 (stream-car clone-667)))
        (let ((var-693 (inner-loop_1-28-484-675 var-674
                                                clone-667
                                                (cons clone-669 clone-668)
                                                clone-667)))
          (let ((var-694 (result-dot var-693)))
            (parse-bar-18-172-659 var-694
                                  (result-lhs var-693)
                                  (result-att var-693)
                                  clone-668
                                  (result-inp var-693)))))
      (loop_6-20-220-696 clone-670 clone-669 clone-668 clone-667)))
(define (loop_6-20-220-696 clone-700 clone-699 clone-698 clone-697)
  (if (equal? 't clone-700)
      (let ((var-704 (stream-car clone-697)))
        (let ((var-705 (inner-loop_1-28-484-620 var-704
                                                clone-697
                                                (cons clone-699 clone-698)
                                                clone-697)))
          (let ((var-706 (result-dot var-705)))
            (parse-bar-18-172-659 var-706
                                  (result-lhs var-705)
                                  (result-att var-705)
                                  clone-698
                                  (result-inp var-705)))))
      (let ((var-711 (stream-car clone-697)))
        (let ((var-1582 (inner-loop_1-28-484-712 var-711
                                                 clone-697
                                                 (cons clone-699 clone-698)
                                                 clone-697)))
          (let ((var-1583 (result-dot var-1582)))
            (parse-bar-18-172-659 var-1583
                                  (result-lhs var-1582)
                                  (result-att var-1582)
                                  clone-698
                                  (result-inp var-1582)))))))
(define (inner-loop_1-28-484-712 clone-716 clone-715 clone-714 clone-713)
  (let ((var-718 (equal? clone-716 '$)))
    (if (loop_4-30-500-719 var-718 clone-716)
        (parse-result 't 1 ((lambda ($1) $1) (car clone-714)) clone-715)
        (let ((var-739 (stream-car clone-715)))
          (let ((var-740 (car var-739)))
            (loop_2-33-338-741 var-740 var-739 clone-714 clone-715))))))
(define (loop_2-33-338-741 clone-745 clone-744 clone-743 clone-742)
  (if (equal? '/ clone-745)
      (let ((var-747 (stream-cdr clone-742)))
        (let ((var-748 (stream-car var-747)))
          (let ((var-749 (car var-748)))
            (let ((var-1567 (loop_5-22-119-750 var-749
                                               var-748
                                               (cons (cdr clone-744) clone-743)
                                               var-747)))
              (let ((var-1568 (result-dot var-1567)))
                (parse-result (result-lhs var-1567)
                              (- var-1568 1)
                              (result-att var-1567)
                              (result-inp var-1567)))))))
      (loop_2-33-338-1570 clone-745 clone-744 clone-743 clone-742)))
(define (loop_2-33-338-1570 clone-1574 clone-1573 clone-1572 clone-1571)
  (if (equal? '* clone-1574)
      (let ((var-1576 (stream-cdr clone-1571)))
        (let ((var-1577 (stream-car var-1576)))
          (let ((var-1578 (car var-1577)))
            (let ((var-1579 (loop_5-22-119-839 var-1578
                                               var-1577
                                               (cons (cdr clone-1573)
                                                     clone-1572)
                                               var-1576)))
              (let ((var-1580 (result-dot var-1579)))
                (parse-result (result-lhs var-1579)
                              (- var-1580 1)
                              (result-att var-1579)
                              (result-inp var-1579)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1574)))
(define (loop_5-22-119-750 clone-754 clone-753 clone-752 clone-751)
  (if (equal? 'n clone-754)
      (let ((var-756 (stream-cdr clone-751)))
        (let ((var-758 (stream-car var-756)))
          (let ((var-759 (inner-loop_1-28-484-551 var-758
                                                  var-756
                                                  (cons (cdr clone-753)
                                                        clone-752)
                                                  var-756)))
            (let ((var-760 (result-dot var-759)))
              (parse-bar-18-172-761 var-760
                                    (result-lhs var-759)
                                    (result-att var-759)
                                    clone-752
                                    (result-inp var-759))))))
      (loop_5-22-119-1555 clone-754 clone-753 clone-752 clone-751)))
(define (loop_5-22-119-1555 clone-1559 clone-1558 clone-1557 clone-1556)
  (if (equal? 'l clone-1559)
      (let ((var-1561 (stream-cdr clone-1556)))
        (let ((var-1562 (stream-car var-1561)))
          (let ((var-1563 (car var-1562)))
            (let ((var-1564 (loop_5-22-119-903 var-1563
                                               var-1562
                                               (cons (cdr clone-1558)
                                                     clone-1557)
                                               var-1561)))
              (let ((var-1565 (result-dot var-1564)))
                (parse-bar-18-172-761 var-1565
                                      (result-lhs var-1564)
                                      (result-att var-1564)
                                      clone-1557
                                      (result-inp var-1564)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1559)))
(define (parse-bar-18-172-761 clone-766 clone-765 clone-764 clone-763 clone-762)
  (if (< 1 clone-766)
      (parse-result clone-765 (- clone-766 1) clone-764 clone-762)
      (loop_6-20-220-768 clone-765 clone-764 clone-763 clone-762)))
(define (loop_6-20-220-768 clone-772 clone-771 clone-770 clone-769)
  (if (equal? 't clone-772)
      (let ((var-776 (stream-car clone-769)))
        (let ((var-795 (inner-loop_1-28-484-777 var-776
                                                clone-769
                                                (cons clone-771 clone-770)
                                                clone-769)))
          (let ((var-796 (result-dot var-795)))
            (parse-bar-18-172-761 var-796
                                  (result-lhs var-795)
                                  (result-att var-795)
                                  clone-770
                                  (result-inp var-795)))))
      (let ((var-801 (stream-car clone-769)))
        (let ((var-1552 (inner-loop_1-28-484-802 var-801
                                                 clone-769
                                                 (cons clone-771 clone-770)
                                                 clone-769)))
          (let ((var-1553 (result-dot var-1552)))
            (parse-bar-18-172-761 var-1553
                                  (result-lhs var-1552)
                                  (result-att var-1552)
                                  clone-770
                                  (result-inp var-1552)))))))
(define (inner-loop_1-28-484-802 clone-806 clone-805 clone-804 clone-803)
  (let ((var-808 (equal? clone-806 '$)))
    (if (loop_4-30-500-719 var-808 clone-806)
        (parse-result 't 1 ((lambda ($1) $1) (car clone-804)) clone-805)
        (let ((var-816 (stream-car clone-805)))
          (let ((var-817 (car var-816)))
            (loop_2-33-338-818 var-817 var-816 clone-804 clone-805))))))
(define (loop_2-33-338-818 clone-822 clone-821 clone-820 clone-819)
  (if (equal? '/ clone-822)
      (let ((var-824 (stream-cdr clone-819)))
        (let ((var-825 (stream-car var-824)))
          (let ((var-826 (car var-825)))
            (let ((var-827 (loop_5-22-119-750 var-826
                                              var-825
                                              (cons (cdr clone-821) clone-820)
                                              var-824)))
              (let ((var-828 (result-dot var-827)))
                (parse-result (result-lhs var-827)
                              (- var-828 1)
                              (result-att var-827)
                              (result-inp var-827)))))))
      (loop_2-33-338-830 clone-822 clone-821 clone-820 clone-819)))
(define (loop_2-33-338-830 clone-834 clone-833 clone-832 clone-831)
  (if (equal? '* clone-834)
      (let ((var-836 (stream-cdr clone-831)))
        (let ((var-837 (stream-car var-836)))
          (let ((var-838 (car var-837)))
            (let ((var-1549 (loop_5-22-119-839 var-838
                                               var-837
                                               (cons (cdr clone-833) clone-832)
                                               var-836)))
              (let ((var-1550 (result-dot var-1549)))
                (parse-result (result-lhs var-1549)
                              (- var-1550 1)
                              (result-att var-1549)
                              (result-inp var-1549)))))))
      (_sim-error 'direct-parse "can't shift on" clone-834)))
(define (loop_5-22-119-839 clone-843 clone-842 clone-841 clone-840)
  (if (equal? 'n clone-843)
      (let ((var-845 (stream-cdr clone-840)))
        (let ((var-847 (stream-car var-845)))
          (let ((var-848 (inner-loop_1-28-484-551 var-847
                                                  var-845
                                                  (cons (cdr clone-842)
                                                        clone-841)
                                                  var-845)))
            (let ((var-849 (result-dot var-848)))
              (parse-bar-18-172-850 var-849
                                    (result-lhs var-848)
                                    (result-att var-848)
                                    clone-841
                                    (result-inp var-848))))))
      (loop_5-22-119-894 clone-843 clone-842 clone-841 clone-840)))
(define (loop_5-22-119-894 clone-898 clone-897 clone-896 clone-895)
  (if (equal? 'l clone-898)
      (let ((var-900 (stream-cdr clone-895)))
        (let ((var-901 (stream-car var-900)))
          (let ((var-902 (car var-901)))
            (let ((var-1546 (loop_5-22-119-903 var-902
                                               var-901
                                               (cons (cdr clone-897) clone-896)
                                               var-900)))
              (let ((var-1547 (result-dot var-1546)))
                (parse-bar-18-172-850 var-1547
                                      (result-lhs var-1546)
                                      (result-att var-1546)
                                      clone-896
                                      (result-inp var-1546)))))))
      (_sim-error 'direct-parse "can't shift on" clone-898)))
(define (loop_5-22-119-903 clone-907 clone-906 clone-905 clone-904)
  (if (equal? 'n clone-907)
      (let ((var-909 (stream-cdr clone-904)))
        (let ((var-911 (stream-car var-909)))
          (let ((var-951 (inner-loop_1-28-484-912 var-911
                                                  var-909
                                                  (cons (cdr clone-906)
                                                        clone-905)
                                                  var-909)))
            (let ((var-952 (result-dot var-951)))
              (parse-bar-18-172-953 var-952
                                    (result-lhs var-951)
                                    (result-att var-951)
                                    clone-905
                                    (result-inp var-951))))))
      (loop_5-22-119-1534 clone-907 clone-906 clone-905 clone-904)))
(define (loop_5-22-119-1534 clone-1538 clone-1537 clone-1536 clone-1535)
  (if (equal? 'l clone-1538)
      (let ((var-1540 (stream-cdr clone-1535)))
        (let ((var-1541 (stream-car var-1540)))
          (let ((var-1542 (car var-1541)))
            (let ((var-1543 (loop_5-22-119-1288 var-1542
                                                var-1541
                                                (cons (cdr clone-1537)
                                                      clone-1536)
                                                var-1540)))
              (let ((var-1544 (result-dot var-1543)))
                (parse-bar-18-172-953 var-1544
                                      (result-lhs var-1543)
                                      (result-att var-1543)
                                      clone-1536
                                      (result-inp var-1543)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1538)))
(define (parse-bar-18-172-953 clone-958 clone-957 clone-956 clone-955 clone-954)
  (if (< 1 clone-958)
      (parse-result clone-957 (- clone-958 1) clone-956 clone-954)
      (loop_6-20-220-960 clone-957 clone-956 clone-955 clone-954)))
(define (loop_6-20-220-960 clone-964 clone-963 clone-962 clone-961)
  (if (equal? 'e clone-964)
      (let ((var-967 (stream-car clone-961)))
        (let ((var-968 (car var-967)))
          (let ((var-999 (loop_5-22-119-969 var-968
                                            var-967
                                            (cons clone-963 clone-962)
                                            clone-961)))
            (let ((var-1000 (result-dot var-999)))
              (parse-bar-18-172-953 var-1000
                                    (result-lhs var-999)
                                    (result-att var-999)
                                    clone-962
                                    (result-inp var-999))))))
      (loop_6-20-220-1002 clone-964 clone-963 clone-962 clone-961)))
(define (loop_6-20-220-1002 clone-1006 clone-1005 clone-1004 clone-1003)
  (if (equal? 't clone-1006)
      (let ((var-1010 (stream-car clone-1003)))
        (let ((var-1524 (inner-loop_1-28-484-1011 var-1010
                                                  clone-1003
                                                  (cons clone-1005 clone-1004)
                                                  clone-1003)))
          (let ((var-1525 (result-dot var-1524)))
            (parse-bar-18-172-953 var-1525
                                  (result-lhs var-1524)
                                  (result-att var-1524)
                                  clone-1004
                                  (result-inp var-1524)))))
      (let ((var-1530 (stream-car clone-1003)))
        (let ((var-1531 (inner-loop_1-28-484-1100 var-1530
                                                  clone-1003
                                                  (cons clone-1005 clone-1004)
                                                  clone-1003)))
          (let ((var-1532 (result-dot var-1531)))
            (parse-bar-18-172-953 var-1532
                                  (result-lhs var-1531)
                                  (result-att var-1531)
                                  clone-1004
                                  (result-inp var-1531)))))))
(define (inner-loop_1-28-484-1011 clone-1015 clone-1014 clone-1013 clone-1012)
  (let ((var-1017 (equal? clone-1015 'r)))
    (if (loop_4-30-500-939 var-1017 clone-1015)
        (parse-result 'e 1 ((lambda ($1) $1) (car clone-1013)) clone-1014)
        (let ((var-1025 (stream-car clone-1014)))
          (let ((var-1026 (car var-1025)))
            (loop_2-33-338-1027 var-1026 var-1025 clone-1013 clone-1014))))))
(define (loop_2-33-338-1027 clone-1031 clone-1030 clone-1029 clone-1028)
  (if (equal? '- clone-1031)
      (let ((var-1033 (stream-cdr clone-1028)))
        (let ((var-1034 (stream-car var-1033)))
          (let ((var-1035 (car var-1034)))
            (let ((var-1430 (loop_5-22-119-1036 var-1035
                                                var-1034
                                                (cons (cdr clone-1030)
                                                      clone-1029)
                                                var-1033)))
              (let ((var-1431 (result-dot var-1430)))
                (parse-result (result-lhs var-1430)
                              (- var-1431 1)
                              (result-att var-1430)
                              (result-inp var-1430)))))))
      (loop_2-33-338-1433 clone-1031 clone-1030 clone-1029 clone-1028)))
(define (loop_2-33-338-1433 clone-1437 clone-1436 clone-1435 clone-1434)
  (if (equal? '+ clone-1437)
      (let ((var-1439 (stream-cdr clone-1434)))
        (let ((var-1440 (stream-car var-1439)))
          (let ((var-1441 (car var-1440)))
            (let ((var-1521 (loop_5-22-119-1442 var-1441
                                                var-1440
                                                (cons (cdr clone-1436)
                                                      clone-1435)
                                                var-1439)))
              (let ((var-1522 (result-dot var-1521)))
                (parse-result (result-lhs var-1521)
                              (- var-1522 1)
                              (result-att var-1521)
                              (result-inp var-1521)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1437)))
(define (loop_5-22-119-1442 clone-1446 clone-1445 clone-1444 clone-1443)
  (if (equal? 'n clone-1446)
      (let ((var-1448 (stream-cdr clone-1443)))
        (let ((var-1450 (stream-car var-1448)))
          (let ((var-1451 (inner-loop_1-28-484-912 var-1450
                                                   var-1448
                                                   (cons (cdr clone-1445)
                                                         clone-1444)
                                                   var-1448)))
            (let ((var-1452 (result-dot var-1451)))
              (parse-bar-18-172-1453 var-1452
                                     (result-lhs var-1451)
                                     (result-att var-1451)
                                     clone-1444
                                     (result-inp var-1451))))))
      (loop_5-22-119-1509 clone-1446 clone-1445 clone-1444 clone-1443)))
(define (loop_5-22-119-1509 clone-1513 clone-1512 clone-1511 clone-1510)
  (if (equal? 'l clone-1513)
      (let ((var-1515 (stream-cdr clone-1510)))
        (let ((var-1516 (stream-car var-1515)))
          (let ((var-1517 (car var-1516)))
            (let ((var-1518 (loop_5-22-119-1288 var-1517
                                                var-1516
                                                (cons (cdr clone-1512)
                                                      clone-1511)
                                                var-1515)))
              (let ((var-1519 (result-dot var-1518)))
                (parse-bar-18-172-1453 var-1519
                                       (result-lhs var-1518)
                                       (result-att var-1518)
                                       clone-1511
                                       (result-inp var-1518)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1513)))
(define (parse-bar-18-172-1453 clone-1458
                               clone-1457
                               clone-1456
                               clone-1455
                               clone-1454)
  (if (< 1 clone-1458)
      (parse-result clone-1457 (- clone-1458 1) clone-1456 clone-1454)
      (loop_6-20-220-1460 clone-1457 clone-1456 clone-1455 clone-1454)))
(define (loop_6-20-220-1460 clone-1464 clone-1463 clone-1462 clone-1461)
  (if (equal? 'e clone-1464)
      (let ((var-1468 (stream-car clone-1461)))
        (let ((var-1487 (inner-loop_1-28-484-1469 var-1468
                                                  clone-1461
                                                  (cons clone-1463 clone-1462)
                                                  clone-1461)))
          (let ((var-1488 (result-dot var-1487)))
            (parse-bar-18-172-1453 var-1488
                                   (result-lhs var-1487)
                                   (result-att var-1487)
                                   clone-1462
                                   (result-inp var-1487)))))
      (loop_6-20-220-1490 clone-1464 clone-1463 clone-1462 clone-1461)))
(define (loop_6-20-220-1490 clone-1494 clone-1493 clone-1492 clone-1491)
  (if (equal? 't clone-1494)
      (let ((var-1498 (stream-car clone-1491)))
        (let ((var-1499 (inner-loop_1-28-484-1011 var-1498
                                                  clone-1491
                                                  (cons clone-1493 clone-1492)
                                                  clone-1491)))
          (let ((var-1500 (result-dot var-1499)))
            (parse-bar-18-172-1453 var-1500
                                   (result-lhs var-1499)
                                   (result-att var-1499)
                                   clone-1492
                                   (result-inp var-1499)))))
      (let ((var-1505 (stream-car clone-1491)))
        (let ((var-1506 (inner-loop_1-28-484-1100 var-1505
                                                  clone-1491
                                                  (cons clone-1493 clone-1492)
                                                  clone-1491)))
          (let ((var-1507 (result-dot var-1506)))
            (parse-bar-18-172-1453 var-1507
                                   (result-lhs var-1506)
                                   (result-att var-1506)
                                   clone-1492
                                   (result-inp var-1506)))))))
(define (inner-loop_1-28-484-1469 clone-1473 clone-1472 clone-1471 clone-1470)
  (let ((var-1475 (equal? clone-1473 'r)))
    (if (loop_4-30-500-939 var-1475 clone-1473)
        (let ((var-1480 (cdr clone-1471)))
          (let ((var-1481 (cdr var-1480)))
            (parse-result 'e
                          3
                          ((lambda ($1 $2 $3) (+ $1 $3))
                           (car var-1481)
                           (car var-1480)
                           (car clone-1471))
                          clone-1472)))
        (let ((var-1485 (stream-car clone-1472)))
          (let ((var-1486 (car var-1485)))
            (_sim-error 'direct-parse "can't shift on" var-1486))))))
(define (loop_5-22-119-1036 clone-1040 clone-1039 clone-1038 clone-1037)
  (if (equal? 'n clone-1040)
      (let ((var-1042 (stream-cdr clone-1037)))
        (let ((var-1044 (stream-car var-1042)))
          (let ((var-1045 (inner-loop_1-28-484-912 var-1044
                                                   var-1042
                                                   (cons (cdr clone-1039)
                                                         clone-1038)
                                                   var-1042)))
            (let ((var-1046 (result-dot var-1045)))
              (parse-bar-18-172-1047 var-1046
                                     (result-lhs var-1045)
                                     (result-att var-1045)
                                     clone-1038
                                     (result-inp var-1045))))))
      (loop_5-22-119-1418 clone-1040 clone-1039 clone-1038 clone-1037)))
(define (loop_5-22-119-1418 clone-1422 clone-1421 clone-1420 clone-1419)
  (if (equal? 'l clone-1422)
      (let ((var-1424 (stream-cdr clone-1419)))
        (let ((var-1425 (stream-car var-1424)))
          (let ((var-1426 (car var-1425)))
            (let ((var-1427 (loop_5-22-119-1288 var-1426
                                                var-1425
                                                (cons (cdr clone-1421)
                                                      clone-1420)
                                                var-1424)))
              (let ((var-1428 (result-dot var-1427)))
                (parse-bar-18-172-1047 var-1428
                                       (result-lhs var-1427)
                                       (result-att var-1427)
                                       clone-1420
                                       (result-inp var-1427)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1422)))
(define (parse-bar-18-172-1047 clone-1052
                               clone-1051
                               clone-1050
                               clone-1049
                               clone-1048)
  (if (< 1 clone-1052)
      (parse-result clone-1051 (- clone-1052 1) clone-1050 clone-1048)
      (loop_6-20-220-1054 clone-1051 clone-1050 clone-1049 clone-1048)))
(define (loop_6-20-220-1054 clone-1058 clone-1057 clone-1056 clone-1055)
  (if (equal? 'e clone-1058)
      (let ((var-1062 (stream-car clone-1055)))
        (let ((var-1081 (inner-loop_1-28-484-1063 var-1062
                                                  clone-1055
                                                  (cons clone-1057 clone-1056)
                                                  clone-1055)))
          (let ((var-1082 (result-dot var-1081)))
            (parse-bar-18-172-1047 var-1082
                                   (result-lhs var-1081)
                                   (result-att var-1081)
                                   clone-1056
                                   (result-inp var-1081)))))
      (loop_6-20-220-1084 clone-1058 clone-1057 clone-1056 clone-1055)))
(define (loop_6-20-220-1084 clone-1088 clone-1087 clone-1086 clone-1085)
  (if (equal? 't clone-1088)
      (let ((var-1092 (stream-car clone-1085)))
        (let ((var-1093 (inner-loop_1-28-484-1011 var-1092
                                                  clone-1085
                                                  (cons clone-1087 clone-1086)
                                                  clone-1085)))
          (let ((var-1094 (result-dot var-1093)))
            (parse-bar-18-172-1047 var-1094
                                   (result-lhs var-1093)
                                   (result-att var-1093)
                                   clone-1086
                                   (result-inp var-1093)))))
      (let ((var-1099 (stream-car clone-1085)))
        (let ((var-1415 (inner-loop_1-28-484-1100 var-1099
                                                  clone-1085
                                                  (cons clone-1087 clone-1086)
                                                  clone-1085)))
          (let ((var-1416 (result-dot var-1415)))
            (parse-bar-18-172-1047 var-1416
                                   (result-lhs var-1415)
                                   (result-att var-1415)
                                   clone-1086
                                   (result-inp var-1415)))))))
(define (inner-loop_1-28-484-1100 clone-1104 clone-1103 clone-1102 clone-1101)
  (let ((var-1106 (equal? clone-1104 '+)))
    (if (loop_4-30-500-1107 var-1106 clone-1104)
        (parse-result 't 1 ((lambda ($1) $1) (car clone-1102)) clone-1103)
        (let ((var-1124 (stream-car clone-1103)))
          (let ((var-1125 (car var-1124)))
            (loop_2-33-338-1126 var-1125 var-1124 clone-1102 clone-1103))))))
(define (loop_2-33-338-1126 clone-1130 clone-1129 clone-1128 clone-1127)
  (if (equal? '/ clone-1130)
      (let ((var-1132 (stream-cdr clone-1127)))
        (let ((var-1133 (stream-car var-1132)))
          (let ((var-1134 (car var-1133)))
            (let ((var-1400 (loop_5-22-119-1135 var-1134
                                                var-1133
                                                (cons (cdr clone-1129)
                                                      clone-1128)
                                                var-1132)))
              (let ((var-1401 (result-dot var-1400)))
                (parse-result (result-lhs var-1400)
                              (- var-1401 1)
                              (result-att var-1400)
                              (result-inp var-1400)))))))
      (loop_2-33-338-1403 clone-1130 clone-1129 clone-1128 clone-1127)))
(define (loop_2-33-338-1403 clone-1407 clone-1406 clone-1405 clone-1404)
  (if (equal? '* clone-1407)
      (let ((var-1409 (stream-cdr clone-1404)))
        (let ((var-1410 (stream-car var-1409)))
          (let ((var-1411 (car var-1410)))
            (let ((var-1412 (loop_5-22-119-1224 var-1411
                                                var-1410
                                                (cons (cdr clone-1406)
                                                      clone-1405)
                                                var-1409)))
              (let ((var-1413 (result-dot var-1412)))
                (parse-result (result-lhs var-1412)
                              (- var-1413 1)
                              (result-att var-1412)
                              (result-inp var-1412)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1407)))
(define (loop_5-22-119-1135 clone-1139 clone-1138 clone-1137 clone-1136)
  (if (equal? 'n clone-1139)
      (let ((var-1141 (stream-cdr clone-1136)))
        (let ((var-1143 (stream-car var-1141)))
          (let ((var-1144 (inner-loop_1-28-484-912 var-1143
                                                   var-1141
                                                   (cons (cdr clone-1138)
                                                         clone-1137)
                                                   var-1141)))
            (let ((var-1145 (result-dot var-1144)))
              (parse-bar-18-172-1146 var-1145
                                     (result-lhs var-1144)
                                     (result-att var-1144)
                                     clone-1137
                                     (result-inp var-1144))))))
      (loop_5-22-119-1388 clone-1139 clone-1138 clone-1137 clone-1136)))
(define (loop_5-22-119-1388 clone-1392 clone-1391 clone-1390 clone-1389)
  (if (equal? 'l clone-1392)
      (let ((var-1394 (stream-cdr clone-1389)))
        (let ((var-1395 (stream-car var-1394)))
          (let ((var-1396 (car var-1395)))
            (let ((var-1397 (loop_5-22-119-1288 var-1396
                                                var-1395
                                                (cons (cdr clone-1391)
                                                      clone-1390)
                                                var-1394)))
              (let ((var-1398 (result-dot var-1397)))
                (parse-bar-18-172-1146 var-1398
                                       (result-lhs var-1397)
                                       (result-att var-1397)
                                       clone-1390
                                       (result-inp var-1397)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1392)))
(define (parse-bar-18-172-1146 clone-1151
                               clone-1150
                               clone-1149
                               clone-1148
                               clone-1147)
  (if (< 1 clone-1151)
      (parse-result clone-1150 (- clone-1151 1) clone-1149 clone-1147)
      (loop_6-20-220-1153 clone-1150 clone-1149 clone-1148 clone-1147)))
(define (loop_6-20-220-1153 clone-1157 clone-1156 clone-1155 clone-1154)
  (if (equal? 't clone-1157)
      (let ((var-1161 (stream-car clone-1154)))
        (let ((var-1180 (inner-loop_1-28-484-1162 var-1161
                                                  clone-1154
                                                  (cons clone-1156 clone-1155)
                                                  clone-1154)))
          (let ((var-1181 (result-dot var-1180)))
            (parse-bar-18-172-1146 var-1181
                                   (result-lhs var-1180)
                                   (result-att var-1180)
                                   clone-1155
                                   (result-inp var-1180)))))
      (let ((var-1186 (stream-car clone-1154)))
        (let ((var-1385 (inner-loop_1-28-484-1187 var-1186
                                                  clone-1154
                                                  (cons clone-1156 clone-1155)
                                                  clone-1154)))
          (let ((var-1386 (result-dot var-1385)))
            (parse-bar-18-172-1146 var-1386
                                   (result-lhs var-1385)
                                   (result-att var-1385)
                                   clone-1155
                                   (result-inp var-1385)))))))
(define (inner-loop_1-28-484-1187 clone-1191 clone-1190 clone-1189 clone-1188)
  (let ((var-1193 (equal? clone-1191 '+)))
    (if (loop_4-30-500-1107 var-1193 clone-1191)
        (parse-result 't 1 ((lambda ($1) $1) (car clone-1189)) clone-1190)
        (let ((var-1201 (stream-car clone-1190)))
          (let ((var-1202 (car var-1201)))
            (loop_2-33-338-1203 var-1202 var-1201 clone-1189 clone-1190))))))
(define (loop_2-33-338-1203 clone-1207 clone-1206 clone-1205 clone-1204)
  (if (equal? '/ clone-1207)
      (let ((var-1209 (stream-cdr clone-1204)))
        (let ((var-1210 (stream-car var-1209)))
          (let ((var-1211 (car var-1210)))
            (let ((var-1212 (loop_5-22-119-1135 var-1211
                                                var-1210
                                                (cons (cdr clone-1206)
                                                      clone-1205)
                                                var-1209)))
              (let ((var-1213 (result-dot var-1212)))
                (parse-result (result-lhs var-1212)
                              (- var-1213 1)
                              (result-att var-1212)
                              (result-inp var-1212)))))))
      (loop_2-33-338-1215 clone-1207 clone-1206 clone-1205 clone-1204)))
(define (loop_2-33-338-1215 clone-1219 clone-1218 clone-1217 clone-1216)
  (if (equal? '* clone-1219)
      (let ((var-1221 (stream-cdr clone-1216)))
        (let ((var-1222 (stream-car var-1221)))
          (let ((var-1223 (car var-1222)))
            (let ((var-1382 (loop_5-22-119-1224 var-1223
                                                var-1222
                                                (cons (cdr clone-1218)
                                                      clone-1217)
                                                var-1221)))
              (let ((var-1383 (result-dot var-1382)))
                (parse-result (result-lhs var-1382)
                              (- var-1383 1)
                              (result-att var-1382)
                              (result-inp var-1382)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1219)))
(define (loop_5-22-119-1224 clone-1228 clone-1227 clone-1226 clone-1225)
  (if (equal? 'n clone-1228)
      (let ((var-1230 (stream-cdr clone-1225)))
        (let ((var-1232 (stream-car var-1230)))
          (let ((var-1233 (inner-loop_1-28-484-912 var-1232
                                                   var-1230
                                                   (cons (cdr clone-1227)
                                                         clone-1226)
                                                   var-1230)))
            (let ((var-1234 (result-dot var-1233)))
              (parse-bar-18-172-1235 var-1234
                                     (result-lhs var-1233)
                                     (result-att var-1233)
                                     clone-1226
                                     (result-inp var-1233))))))
      (loop_5-22-119-1279 clone-1228 clone-1227 clone-1226 clone-1225)))
(define (loop_5-22-119-1279 clone-1283 clone-1282 clone-1281 clone-1280)
  (if (equal? 'l clone-1283)
      (let ((var-1285 (stream-cdr clone-1280)))
        (let ((var-1286 (stream-car var-1285)))
          (let ((var-1287 (car var-1286)))
            (let ((var-1379 (loop_5-22-119-1288 var-1287
                                                var-1286
                                                (cons (cdr clone-1282)
                                                      clone-1281)
                                                var-1285)))
              (let ((var-1380 (result-dot var-1379)))
                (parse-bar-18-172-1235 var-1380
                                       (result-lhs var-1379)
                                       (result-att var-1379)
                                       clone-1281
                                       (result-inp var-1379)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1283)))
(define (loop_5-22-119-1288 clone-1292 clone-1291 clone-1290 clone-1289)
  (if (equal? 'n clone-1292)
      (let ((var-1294 (stream-cdr clone-1289)))
        (let ((var-1296 (stream-car var-1294)))
          (let ((var-1297 (inner-loop_1-28-484-912 var-1296
                                                   var-1294
                                                   (cons (cdr clone-1291)
                                                         clone-1290)
                                                   var-1294)))
            (let ((var-1298 (result-dot var-1297)))
              (parse-bar-18-172-1299 var-1298
                                     (result-lhs var-1297)
                                     (result-att var-1297)
                                     clone-1290
                                     (result-inp var-1297))))))
      (loop_5-22-119-1367 clone-1292 clone-1291 clone-1290 clone-1289)))
(define (loop_5-22-119-1367 clone-1371 clone-1370 clone-1369 clone-1368)
  (if (equal? 'l clone-1371)
      (let ((var-1373 (stream-cdr clone-1368)))
        (let ((var-1374 (stream-car var-1373)))
          (let ((var-1375 (car var-1374)))
            (let ((var-1376 (loop_5-22-119-1288 var-1375
                                                var-1374
                                                (cons (cdr clone-1370)
                                                      clone-1369)
                                                var-1373)))
              (let ((var-1377 (result-dot var-1376)))
                (parse-bar-18-172-1299 var-1377
                                       (result-lhs var-1376)
                                       (result-att var-1376)
                                       clone-1369
                                       (result-inp var-1376)))))))
      (_sim-error 'direct-parse "can't shift on" clone-1371)))
(define (parse-bar-18-172-1299 clone-1304
                               clone-1303
                               clone-1302
                               clone-1301
                               clone-1300)
  (if (< 1 clone-1304)
      (parse-result clone-1303 (- clone-1304 1) clone-1302 clone-1300)
      (loop_6-20-220-1306 clone-1303 clone-1302 clone-1301 clone-1300)))
(define (loop_6-20-220-1306 clone-1310 clone-1309 clone-1308 clone-1307)
  (if (equal? 'e clone-1310)
      (let ((var-1313 (stream-car clone-1307)))
        (let ((var-1314 (car var-1313)))
          (let ((var-1345 (loop_5-22-119-1315 var-1314
                                              var-1313
                                              (cons clone-1309 clone-1308)
                                              clone-1307)))
            (let ((var-1346 (result-dot var-1345)))
              (parse-bar-18-172-1299 var-1346
                                     (result-lhs var-1345)
                                     (result-att var-1345)
                                     clone-1308
                                     (result-inp var-1345))))))
      (loop_6-20-220-1348 clone-1310 clone-1309 clone-1308 clone-1307)))
(define (loop_6-20-220-1348 clone-1352 clone-1351 clone-1350 clone-1349)
  (if (equal? 't clone-1352)
      (let ((var-1356 (stream-car clone-1349)))
        (let ((var-1357 (inner-loop_1-28-484-1011 var-1356
                                                  clone-1349
                                                  (cons clone-1351 clone-1350)
                                                  clone-1349)))
          (let ((var-1358 (result-dot var-1357)))
            (parse-bar-18-172-1299 var-1358
                                   (result-lhs var-1357)
                                   (result-att var-1357)
                                   clone-1350
                                   (result-inp var-1357)))))
      (let ((var-1363 (stream-car clone-1349)))
        (let ((var-1364 (inner-loop_1-28-484-1100 var-1363
                                                  clone-1349
                                                  (cons clone-1351 clone-1350)
                                                  clone-1349)))
          (let ((var-1365 (result-dot var-1364)))
            (parse-bar-18-172-1299 var-1365
                                   (result-lhs var-1364)
                                   (result-att var-1364)
                                   clone-1350
                                   (result-inp var-1364)))))))
(define (loop_5-22-119-1315 clone-1319 clone-1318 clone-1317 clone-1316)
  (if (equal? 'r clone-1319)
      (let ((var-1321 (stream-cdr clone-1316)))
        (let ((var-1323 (stream-car var-1321)))
          (let ((var-1342 (inner-loop_1-28-484-1324 var-1323
                                                    var-1321
                                                    (cons (cdr clone-1318)
                                                          clone-1317)
                                                    var-1321)))
            (let ((var-1343 (result-dot var-1342)))
              (parse-result (result-lhs var-1342)
                            (- var-1343 1)
                            (result-att var-1342)
                            (result-inp var-1342))))))
      (_sim-error 'direct-parse "can't shift on" clone-1319)))
(define (inner-loop_1-28-484-1324 clone-1328 clone-1327 clone-1326 clone-1325)
  (let ((var-1330 (equal? clone-1328 '*)))
    (if (loop_4-30-500-919 var-1330 clone-1328)
        (let ((var-1335 (cdr clone-1326)))
          (let ((var-1336 (cdr var-1335)))
            (parse-result 'p
                          3
                          ((lambda ($1 $2 $3) $2)
                           (car var-1336)
                           (car var-1335)
                           (car clone-1326))
                          clone-1327)))
        (let ((var-1340 (stream-car clone-1327)))
          (let ((var-1341 (car var-1340)))
            (_sim-error 'direct-parse "can't shift on" var-1341))))))
(define (parse-bar-18-172-1235 clone-1240
                               clone-1239
                               clone-1238
                               clone-1237
                               clone-1236)
  (if (< 1 clone-1240)
      (parse-result clone-1239 (- clone-1240 1) clone-1238 clone-1236)
      (loop_6-20-220-1242 clone-1239 clone-1238 clone-1237 clone-1236)))
(define (loop_6-20-220-1242 clone-1246 clone-1245 clone-1244 clone-1243)
  (if (equal? 't clone-1246)
      (let ((var-1250 (stream-car clone-1243)))
        (let ((var-1269 (inner-loop_1-28-484-1251 var-1250
                                                  clone-1243
                                                  (cons clone-1245 clone-1244)
                                                  clone-1243)))
          (let ((var-1270 (result-dot var-1269)))
            (parse-bar-18-172-1235 var-1270
                                   (result-lhs var-1269)
                                   (result-att var-1269)
                                   clone-1244
                                   (result-inp var-1269)))))
      (let ((var-1275 (stream-car clone-1243)))
        (let ((var-1276 (inner-loop_1-28-484-1187 var-1275
                                                  clone-1243
                                                  (cons clone-1245 clone-1244)
                                                  clone-1243)))
          (let ((var-1277 (result-dot var-1276)))
            (parse-bar-18-172-1235 var-1277
                                   (result-lhs var-1276)
                                   (result-att var-1276)
                                   clone-1244
                                   (result-inp var-1276)))))))
(define (inner-loop_1-28-484-1251 clone-1255 clone-1254 clone-1253 clone-1252)
  (let ((var-1257 (equal? clone-1255 '+)))
    (if (loop_4-30-500-1107 var-1257 clone-1255)
        (let ((var-1262 (cdr clone-1253)))
          (let ((var-1263 (cdr var-1262)))
            (parse-result 't
                          3
                          ((lambda ($1 $2 $3) (* $1 $3))
                           (car var-1263)
                           (car var-1262)
                           (car clone-1253))
                          clone-1254)))
        (let ((var-1267 (stream-car clone-1254)))
          (let ((var-1268 (car var-1267)))
            (_sim-error 'direct-parse "can't shift on" var-1268))))))
(define (inner-loop_1-28-484-1162 clone-1166 clone-1165 clone-1164 clone-1163)
  (let ((var-1168 (equal? clone-1166 '+)))
    (if (loop_4-30-500-1107 var-1168 clone-1166)
        (let ((var-1173 (cdr clone-1164)))
          (let ((var-1174 (cdr var-1173)))
            (parse-result 't
                          3
                          ((lambda ($1 $2 $3) (/ $1 $3))
                           (car var-1174)
                           (car var-1173)
                           (car clone-1164))
                          clone-1165)))
        (let ((var-1178 (stream-car clone-1165)))
          (let ((var-1179 (car var-1178)))
            (_sim-error 'direct-parse "can't shift on" var-1179))))))
(define (loop_4-30-500-1107 clone-1109 clone-1108)
  (if clone-1109
      clone-1109
      (let ((var-1111 (equal? clone-1108 '-)))
        (loop_4-30-500-1112 var-1111 clone-1108))))
(define (loop_4-30-500-1112 clone-1114 clone-1113)
  (if clone-1114
      clone-1114
      (let ((var-1116 (equal? clone-1113 'r)))
        (loop_4-30-500-939 var-1116 clone-1113))))
(define (inner-loop_1-28-484-1063 clone-1067 clone-1066 clone-1065 clone-1064)
  (let ((var-1069 (equal? clone-1067 'r)))
    (if (loop_4-30-500-939 var-1069 clone-1067)
        (let ((var-1074 (cdr clone-1065)))
          (let ((var-1075 (cdr var-1074)))
            (parse-result 'e
                          3
                          ((lambda ($1 $2 $3) (- $1 $3))
                           (car var-1075)
                           (car var-1074)
                           (car clone-1065))
                          clone-1066)))
        (let ((var-1079 (stream-car clone-1066)))
          (let ((var-1080 (car var-1079)))
            (_sim-error 'direct-parse "can't shift on" var-1080))))))
(define (loop_5-22-119-969 clone-973 clone-972 clone-971 clone-970)
  (if (equal? 'r clone-973)
      (let ((var-975 (stream-cdr clone-970)))
        (let ((var-977 (stream-car var-975)))
          (let ((var-996 (inner-loop_1-28-484-978 var-977
                                                  var-975
                                                  (cons (cdr clone-972)
                                                        clone-971)
                                                  var-975)))
            (let ((var-997 (result-dot var-996)))
              (parse-result (result-lhs var-996)
                            (- var-997 1)
                            (result-att var-996)
                            (result-inp var-996))))))
      (_sim-error 'direct-parse "can't shift on" clone-973)))
(define (inner-loop_1-28-484-978 clone-982 clone-981 clone-980 clone-979)
  (let ((var-984 (equal? clone-982 '$)))
    (if (loop_4-30-500-558 var-984 clone-982)
        (let ((var-989 (cdr clone-980)))
          (let ((var-990 (cdr var-989)))
            (parse-result 'p
                          3
                          ((lambda ($1 $2 $3) $2)
                           (car var-990)
                           (car var-989)
                           (car clone-980))
                          clone-981)))
        (let ((var-994 (stream-car clone-981)))
          (let ((var-995 (car var-994)))
            (_sim-error 'direct-parse "can't shift on" var-995))))))
(define (inner-loop_1-28-484-912 clone-916 clone-915 clone-914 clone-913)
  (let ((var-918 (equal? clone-916 '*)))
    (if (loop_4-30-500-919 var-918 clone-916)
        (parse-result 'p 1 ((lambda ($1) $1) (car clone-914)) clone-915)
        (let ((var-949 (stream-car clone-915)))
          (let ((var-950 (car var-949)))
            (_sim-error 'direct-parse "can't shift on" var-950))))))
(define (loop_4-30-500-919 clone-921 clone-920)
  (if clone-921
      clone-921
      (let ((var-923 (equal? clone-920 '+)))
        (loop_4-30-500-924 var-923 clone-920))))
(define (loop_4-30-500-924 clone-926 clone-925)
  (if clone-926
      clone-926
      (let ((var-928 (equal? clone-925 '-)))
        (loop_4-30-500-929 var-928 clone-925))))
(define (loop_4-30-500-929 clone-931 clone-930)
  (if clone-931
      clone-931
      (let ((var-933 (equal? clone-930 '/)))
        (loop_4-30-500-934 var-933 clone-930))))
(define (loop_4-30-500-934 clone-936 clone-935)
  (if clone-936
      clone-936
      (let ((var-938 (equal? clone-935 'r)))
        (loop_4-30-500-939 var-938 clone-935))))
(define (loop_4-30-500-939 clone-941 clone-940)
  (if clone-941 clone-941 #f))
(define (parse-bar-18-172-850 clone-855 clone-854 clone-853 clone-852 clone-851)
  (if (< 1 clone-855)
      (parse-result clone-854 (- clone-855 1) clone-853 clone-851)
      (loop_6-20-220-857 clone-854 clone-853 clone-852 clone-851)))
(define (loop_6-20-220-857 clone-861 clone-860 clone-859 clone-858)
  (if (equal? 't clone-861)
      (let ((var-865 (stream-car clone-858)))
        (let ((var-884 (inner-loop_1-28-484-866 var-865
                                                clone-858
                                                (cons clone-860 clone-859)
                                                clone-858)))
          (let ((var-885 (result-dot var-884)))
            (parse-bar-18-172-850 var-885
                                  (result-lhs var-884)
                                  (result-att var-884)
                                  clone-859
                                  (result-inp var-884)))))
      (let ((var-890 (stream-car clone-858)))
        (let ((var-891 (inner-loop_1-28-484-802 var-890
                                                clone-858
                                                (cons clone-860 clone-859)
                                                clone-858)))
          (let ((var-892 (result-dot var-891)))
            (parse-bar-18-172-850 var-892
                                  (result-lhs var-891)
                                  (result-att var-891)
                                  clone-859
                                  (result-inp var-891)))))))
(define (inner-loop_1-28-484-866 clone-870 clone-869 clone-868 clone-867)
  (let ((var-872 (equal? clone-870 '$)))
    (if (loop_4-30-500-719 var-872 clone-870)
        (let ((var-877 (cdr clone-868)))
          (let ((var-878 (cdr var-877)))
            (parse-result 't
                          3
                          ((lambda ($1 $2 $3) (* $1 $3))
                           (car var-878)
                           (car var-877)
                           (car clone-868))
                          clone-869)))
        (let ((var-882 (stream-car clone-869)))
          (let ((var-883 (car var-882)))
            (_sim-error 'direct-parse "can't shift on" var-883))))))
(define (inner-loop_1-28-484-777 clone-781 clone-780 clone-779 clone-778)
  (let ((var-783 (equal? clone-781 '$)))
    (if (loop_4-30-500-719 var-783 clone-781)
        (let ((var-788 (cdr clone-779)))
          (let ((var-789 (cdr var-788)))
            (parse-result 't
                          3
                          ((lambda ($1 $2 $3) (/ $1 $3))
                           (car var-789)
                           (car var-788)
                           (car clone-779))
                          clone-780)))
        (let ((var-793 (stream-car clone-780)))
          (let ((var-794 (car var-793)))
            (_sim-error 'direct-parse "can't shift on" var-794))))))
(define (loop_4-30-500-719 clone-721 clone-720)
  (if clone-721
      clone-721
      (let ((var-723 (equal? clone-720 '+)))
        (loop_4-30-500-724 var-723 clone-720))))
(define (loop_4-30-500-724 clone-726 clone-725)
  (if clone-726
      clone-726
      (let ((var-728 (equal? clone-725 '-)))
        (loop_4-30-500-729 var-728 clone-725))))
(define (loop_4-30-500-729 clone-731 clone-730)
  (if clone-731 clone-731 #f))
(define (inner-loop_1-28-484-675 clone-679 clone-678 clone-677 clone-676)
  (let ((var-681 (equal? clone-679 '$)))
    (if (loop_4-30-500-627 var-681 clone-679)
        (let ((var-686 (cdr clone-677)))
          (let ((var-687 (cdr var-686)))
            (parse-result 'e
                          3
                          ((lambda ($1 $2 $3) (- $1 $3))
                           (car var-687)
                           (car var-686)
                           (car clone-677))
                          clone-678)))
        (let ((var-691 (stream-car clone-678)))
          (let ((var-692 (car var-691)))
            (_sim-error 'direct-parse "can't shift on" var-692))))))
(define (loop_4-30-500-627 clone-629 clone-628)
  (if clone-629 clone-629 #f))
(define (direct-parse-13-57-606 clone-608 clone-607)
  (if (equal? '$ (car (stream-car clone-607)))
      (car clone-608)
      (_sim-error 'direct-parse "expecting eof" (car (stream-car clone-607)))))
(define (inner-loop_1-28-484-551 clone-555 clone-554 clone-553 clone-552)
  (let ((var-557 (equal? clone-555 '$)))
    (if (loop_4-30-500-558 var-557 clone-555)
        (parse-result 'p 1 ((lambda ($1) $1) (car clone-553)) clone-554)
        (let ((var-588 (stream-car clone-554)))
          (let ((var-589 (car var-588)))
            (_sim-error 'direct-parse "can't shift on" var-589))))))
(define (loop_4-30-500-558 clone-560 clone-559)
  (if clone-560
      clone-560
      (let ((var-562 (equal? clone-559 '*)))
        (loop_4-30-500-563 var-562 clone-559))))
(define (loop_4-30-500-563 clone-565 clone-564)
  (if clone-565
      clone-565
      (let ((var-567 (equal? clone-564 '+)))
        (loop_4-30-500-568 var-567 clone-564))))
(define (loop_4-30-500-568 clone-570 clone-569)
  (if clone-570
      clone-570
      (let ((var-572 (equal? clone-569 '-)))
        (loop_4-30-500-573 var-572 clone-569))))
(define (loop_4-30-500-573 clone-575 clone-574)
  (if clone-575
      clone-575
      (let ((var-577 (equal? clone-574 '/)))
        (loop_4-30-500-578 var-577 clone-574))))
(define (loop_4-30-500-578 clone-580 clone-579)
  (if clone-580 clone-580 #f))
