(define ($goal-1358 clone-1361 clone-1360 clone-1359)
  (_let 1
        clone-1359
        (lambda (var-1364)
          (let ((var-1365 (source-grammar->grammar clone-1361 clone-1360)))
            (let ((var-1366 (car (grammar-productions var-1365))))
              (let ((var-1369 (compute-first var-1365 clone-1360)))
                (let ((var-1370 (list (make-item var-1366
                                                 0
                                                 (cdr (production-rhs var-1366))))))
                  (_let 1
                        (_lift0 1 '())
                        (lambda (var-1371)
                          (_let 1
                                var-1364
                                (lambda (var-1372)
                                  (direct-parse-1335-1373 var-1370
                                                          var-1365
                                                          var-1369
                                                          clone-1360
                                                          var-1371
                                                          var-1372))))))))))))
(define (direct-parse-1335-1373 clone-1379
                                clone-1378
                                clone-1377
                                clone-1375
                                clone-1376
                                clone-1374)
  (if (final? clone-1379 clone-1378)
      (multi-memo 1 'direct-parse-1336-1380 '(1 1) (list clone-1376 clone-1374))
      (let ((var-1385 (compute-closure clone-1379
                                       clone-1378
                                       clone-1375
                                       clone-1377)))
        (let ((var-1386 (next-terminals var-1385 clone-1378)))
          (let ((var-1409 (direct-parse-1337-1387 clone-1379
                                                  clone-1378
                                                  var-1386)))
            (let ((var-1410 (accept var-1385)))
              (direct-parse-1338-1411 var-1409
                                      clone-1378
                                      clone-1377
                                      var-1385
                                      var-1410
                                      clone-1375
                                      clone-1376
                                      clone-1374)))))))
(define (direct-parse-1338-1411 clone-1419
                                clone-1418
                                clone-1417
                                clone-1416
                                clone-1414
                                clone-1413
                                clone-1415
                                clone-1412)
  (if (null? clone-1414)
      (_let 1
            (_op 1 'stream-car clone-1412)
            (lambda (var-1420)
              (_let 1
                    var-1420
                    (lambda (var-1421)
                      (_let 1
                            (_op 1 'car var-1421)
                            (lambda (var-1422)
                              (_let 1
                                    var-1422
                                    (lambda (var-1423)
                                      (_let 1
                                            var-1420
                                            (lambda (var-1428)
                                              (_let 1
                                                    clone-1415
                                                    (lambda (var-1429)
                                                      (_let 1
                                                            clone-1412
                                                            (lambda (var-1430)
                                                              (loop_5-1344-1432 clone-1418
                                                                                clone-1413
                                                                                clone-1417
                                                                                clone-1416
                                                                                clone-1419
                                                                                var-1423
                                                                                var-1428
                                                                                var-1429
                                                                                var-1430)))))))))))))))
      (_let 1
            clone-1412
            (lambda (var-1594)
              (_let 1
                    clone-1415
                    (lambda (var-1595)
                      (_let 1
                            clone-1412
                            (lambda (var-1598)
                              (_let 1
                                    var-1594
                                    (lambda (var-1604)
                                      (_let 1
                                            var-1595
                                            (lambda (var-1605)
                                              (let ((var-1607 (items->trie clone-1414
                                                                           clone-1413)))
                                                (_let 1
                                                      var-1598
                                                      (lambda (var-1609)
                                                        (loop_3-1348-1610 clone-1419
                                                                          clone-1418
                                                                          clone-1413
                                                                          clone-1417
                                                                          clone-1416
                                                                          0
                                                                          clone-1413
                                                                          var-1607
                                                                          var-1604
                                                                          var-1605
                                                                          var-1609))))))))))))))))
(define (loop_3-1348-1610 clone-1621
                          clone-1620
                          clone-1619
                          clone-1618
                          clone-1617
                          clone-1614
                          clone-1613
                          clone-1611
                          clone-1616
                          clone-1615
                          clone-1612)
  (if (null? clone-1611)
      (_let 1
            clone-1616
            (lambda (var-1627)
              (_let 1
                    clone-1615
                    (lambda (var-1628)
                      (continue_1-1346-1630 clone-1621
                                            #f
                                            clone-1620
                                            clone-1619
                                            clone-1618
                                            clone-1617
                                            var-1628
                                            var-1627)))))
      (loop_3-1349-1753 clone-1621
                        clone-1620
                        clone-1619
                        clone-1618
                        clone-1617
                        clone-1614
                        clone-1613
                        clone-1611
                        clone-1616
                        clone-1615
                        clone-1612)))
(define (loop_3-1349-1753 clone-1764
                          clone-1763
                          clone-1762
                          clone-1761
                          clone-1760
                          clone-1757
                          clone-1756
                          clone-1754
                          clone-1759
                          clone-1758
                          clone-1755)
  (if (= clone-1757 clone-1756)
      (_let 1
            clone-1759
            (lambda (var-1770)
              (_let 1
                    clone-1758
                    (lambda (var-1771)
                      (continue_1-1346-1630 clone-1764
                                            clone-1754
                                            clone-1763
                                            clone-1762
                                            clone-1761
                                            clone-1760
                                            var-1771
                                            var-1770)))))
      (_let 1
            (_op 1 'stream-car clone-1755)
            (lambda (var-1773)
              (_let 1
                    var-1773
                    (lambda (var-1774)
                      (_let 1
                            clone-1759
                            (lambda (var-1780)
                              (_let 1
                                    clone-1758
                                    (lambda (var-1781)
                                      (_let 1
                                            clone-1755
                                            (lambda (var-1784)
                                              (inner-loop_1-1350-1786 clone-1764
                                                                      clone-1763
                                                                      clone-1762
                                                                      clone-1761
                                                                      clone-1760
                                                                      clone-1757
                                                                      clone-1756
                                                                      clone-1754
                                                                      var-1774
                                                                      var-1780
                                                                      var-1781
                                                                      var-1784)))))))))))))
(define (inner-loop_1-1350-1786 clone-1797
                                clone-1796
                                clone-1795
                                clone-1794
                                clone-1793
                                clone-1790
                                clone-1789
                                clone-1787
                                clone-1798
                                clone-1792
                                clone-1791
                                clone-1788)
  (if (null? clone-1787)
      (_let 1
            clone-1792
            (lambda (var-1804)
              (_let 1
                    clone-1791
                    (lambda (var-1805)
                      (continue_1-1346-1630 clone-1797
                                            #f
                                            clone-1796
                                            clone-1795
                                            clone-1794
                                            clone-1793
                                            var-1805
                                            var-1804)))))
      (multi-memo 1
                  'inner-loop_1-1351-1807
                  '(0 0 0 0 0 0 0 0 1 1 1 1)
                  (list clone-1797
                        clone-1796
                        clone-1795
                        clone-1794
                        clone-1793
                        clone-1790
                        clone-1789
                        clone-1787
                        clone-1798
                        clone-1792
                        clone-1791
                        clone-1788))))
(define (inner-loop_1-1351-1807 clone-1818
                                clone-1817
                                clone-1816
                                clone-1815
                                clone-1814
                                clone-1811
                                clone-1810
                                clone-1808
                                clone-1819
                                clone-1813
                                clone-1812
                                clone-1809)
  (_let 1
        clone-1819
        (lambda (var-1820)
          (let ((var-1821 (caar clone-1808)))
            (_let 1
                  (_op 1 'equal? var-1820 (_lift0 1 (car var-1821)))
                  (lambda (var-1822)
                    (_if 1
                         (multi-memo 1
                                     'loop_4-1353-1823
                                     '(0 1 1)
                                     (list var-1821 var-1822 var-1820))
                         (lambda ()
                           (_let 1
                                 clone-1813
                                 (lambda (var-1842)
                                   (_let 1
                                         clone-1812
                                         (lambda (var-1843)
                                           (let ((var-1845 (cdar clone-1808)))
                                             (let ((var-1846 (+ clone-1811 1)))
                                               (_let 1
                                                     (inner-loop_1-1352-1833 clone-1811
                                                                             clone-1810
                                                                             clone-1809)
                                                     (lambda (var-1847)
                                                       (loop_3-1348-1610 clone-1818
                                                                         clone-1817
                                                                         clone-1816
                                                                         clone-1815
                                                                         clone-1814
                                                                         var-1846
                                                                         clone-1810
                                                                         var-1845
                                                                         var-1842
                                                                         var-1843
                                                                         var-1847))))))))))
                         (lambda ()
                           (_let 1
                                 clone-1819
                                 (lambda (var-1848)
                                   (_let 1
                                         clone-1813
                                         (lambda (var-1854)
                                           (_let 1
                                                 clone-1812
                                                 (lambda (var-1855)
                                                   (_let 1
                                                         clone-1809
                                                         (lambda (var-1858)
                                                           (let ((var-1859 (cdr clone-1808)))
                                                             (inner-loop_1-1350-1786 clone-1818
                                                                                     clone-1817
                                                                                     clone-1816
                                                                                     clone-1815
                                                                                     clone-1814
                                                                                     clone-1811
                                                                                     clone-1810
                                                                                     var-1859
                                                                                     var-1848
                                                                                     var-1854
                                                                                     var-1855
                                                                                     var-1858))))))))))))))))))
(define (inner-loop_1-1352-1833 clone-1836 clone-1835 clone-1834)
  (if (= (+ clone-1836 1) clone-1835)
      (_lift0 1 '())
      (_op 1 'stream-cdr clone-1834)))
(define (loop_4-1353-1823 clone-1824 clone-1826 clone-1825)
  (_if 1
       clone-1826
       (lambda ()
         clone-1826)
       (lambda ()
         (loop_4-1354-1827 clone-1824 clone-1825))))
(define (loop_4-1354-1827 clone-1828 clone-1829)
  (if (null? (cdr clone-1828))
      (_lift0 1 #f)
      (_let 1
            clone-1829
            (lambda (var-1830)
              (let ((var-1831 (cdr clone-1828)))
                (_let 1
                      (_op 1 'equal? var-1830 (_lift0 1 (car var-1831)))
                      (lambda (var-1832)
                        (multi-memo 1
                                    'loop_4-1353-1823
                                    '(0 1 1)
                                    (list var-1831 var-1832 var-1830)))))))))
(define (continue_1-1346-1630 clone-1638
                              clone-1637
                              clone-1636
                              clone-1635
                              clone-1634
                              clone-1633
                              clone-1632
                              clone-1631)
  (if (not clone-1637)
      (_let 1
            (_op 1 'stream-car clone-1631)
            (lambda (var-1639)
              (_let 1
                    var-1639
                    (lambda (var-1640)
                      (_let 1
                            (_op 1 'car var-1640)
                            (lambda (var-1641)
                              (_let 1
                                    var-1641
                                    (lambda (var-1642)
                                      (_let 1
                                            var-1639
                                            (lambda (var-1647)
                                              (_let 1
                                                    clone-1632
                                                    (lambda (var-1648)
                                                      (_let 1
                                                            clone-1631
                                                            (lambda (var-1649)
                                                              (loop_2-1355-1651 clone-1636
                                                                                clone-1635
                                                                                clone-1634
                                                                                clone-1633
                                                                                clone-1638
                                                                                var-1642
                                                                                var-1647
                                                                                var-1648
                                                                                var-1649)))))))))))))))
      (let ((var-1699 (length (production-rhs clone-1637))))
        (let ((var-1700 (production-lhs clone-1637)))
          (let ((var-1701 (production-attribution clone-1637)))
            (_let 1
                  clone-1632
                  (lambda (var-1703)
                    (let ((var-1712 (top->mylist-1330-1704 var-1699 var-1703)))
                      (let ((var-1714 (static-constructor 'mynil
                                                          mynil
                                                          (list)
                                                          '())))
                        (let ((var-1720 (loop_1-1357-1715 var-1712 var-1714)))
                          (_let 1
                                (_op 1
                                     'apply
                                     var-1701
                                     (my-list->list-1332-1721 var-1720))
                                (lambda (var-1724)
                                  (continue_1-1347-1725 clone-1636
                                                        clone-1635
                                                        clone-1634
                                                        clone-1633
                                                        var-1700
                                                        var-1699
                                                        clone-1632
                                                        var-1724
                                                        clone-1631)))))))))))))
(define (continue_1-1347-1725 clone-1734
                              clone-1733
                              clone-1732
                              clone-1731
                              clone-1729
                              clone-1728
                              clone-1730
                              clone-1727
                              clone-1726)
  (if (zero? clone-1728)
      (_let 1
            clone-1727
            (lambda (var-1740)
              (_let 1
                    clone-1730
                    (lambda (var-1741)
                      (_let 1
                            clone-1726
                            (lambda (var-1742)
                              (let ((var-1743 (goto clone-1731 clone-1729)))
                                (_let 1
                                      var-1740
                                      (lambda (var-1744)
                                        (_let 1
                                              var-1741
                                              (lambda (var-1745)
                                                (_let 1
                                                      (_op 1
                                                           'cons
                                                           var-1744
                                                           var-1745)
                                                      (lambda (var-1750)
                                                        (_let 1
                                                              var-1742
                                                              (lambda (var-1751)
                                                                (_let 1
                                                                      (direct-parse-1335-1373 var-1743
                                                                                              clone-1734
                                                                                              clone-1732
                                                                                              clone-1733
                                                                                              var-1750
                                                                                              var-1751)
                                                                      (lambda (var-1752)
                                                                        (parse-bar-1339-1471 var-1743
                                                                                             clone-1734
                                                                                             clone-1733
                                                                                             clone-1732
                                                                                             clone-1731
                                                                                             var-1752
                                                                                             var-1741))))))))))))))))))
      (_op 1
           'parse-result
           (_lift0 1 clone-1729)
           (_lift0 1 clone-1728)
           clone-1727
           clone-1726)))
(define (my-list->list-1332-1721 clone-1722)
  (if (mynil? (clone-1722 'value))
      (_lift0 1 '())
      (let ((var-1723 (mycdr (clone-1722 'value))))
        (_op 1
             'cons
             (mycar (clone-1722 'value))
             (my-list->list-1332-1721 var-1723)))))
(define (loop_1-1357-1715 clone-1717 clone-1716)
  (if (mynil? (clone-1717 'value))
      clone-1716
      (let ((var-1718 (mycdr (clone-1717 'value))))
        (let ((var-1719 (static-constructor 'mycons
                                            mycons
                                            (list (mycar (clone-1717 'value))
                                                  clone-1716)
                                            '(1 0))))
          (loop_1-1357-1715 var-1718 var-1719)))))
(define (top->mylist-1330-1704 clone-1706 clone-1705)
  (if (zero? clone-1706)
      (static-constructor 'mynil mynil (list) '())
      (top->mylist-1331-1707 clone-1706 clone-1705)))
(define (top->mylist-1331-1707 clone-1709 clone-1708)
  (if (= clone-1709 1)
      (static-constructor 'mycons
                          mycons
                          (list (_op 1 'car clone-1708)
                                (static-constructor 'mynil
                                                    mynil
                                                    (list)
                                                    '()))
                          '(1 0))
      (let ((var-1710 (- clone-1709 1)))
        (_let 1
              (_op 1 'cdr clone-1708)
              (lambda (var-1711)
                (static-constructor 'mycons
                                    mycons
                                    (list (_op 1 'car clone-1708)
                                          (top->mylist-1330-1704 var-1710
                                                                 var-1711))
                                    '(1 0)))))))
(define (loop_2-1355-1651 clone-1659
                          clone-1658
                          clone-1657
                          clone-1656
                          clone-1652
                          clone-1660
                          clone-1655
                          clone-1654
                          clone-1653)
  (if (null? clone-1652)
      (_op 1
           '_sim-error
           (_lift0 1 'direct-parse)
           (_lift0 1 "can't shift on")
           clone-1660)
      (multi-memo 1
                  'loop_2-1356-1661
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-1659
                        clone-1658
                        clone-1657
                        clone-1656
                        clone-1652
                        clone-1660
                        clone-1655
                        clone-1654
                        clone-1653))))
(define (loop_2-1356-1661 clone-1669
                          clone-1668
                          clone-1667
                          clone-1666
                          clone-1662
                          clone-1670
                          clone-1665
                          clone-1664
                          clone-1663)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-1662)) clone-1670)
       (lambda ()
         (_let 1
               clone-1665
               (lambda (var-1671)
                 (let ((var-1676 (car clone-1662)))
                   (_let 1
                         (_op 1 'cdr var-1671)
                         (lambda (var-1677)
                           (_let 1
                                 clone-1664
                                 (lambda (var-1678)
                                   (_let 1
                                         (_op 1 'stream-cdr clone-1663)
                                         (lambda (var-1679)
                                           (let ((var-1680 (goto clone-1666
                                                                 var-1676)))
                                             (_let 1
                                                   var-1677
                                                   (lambda (var-1681)
                                                     (_let 1
                                                           var-1678
                                                           (lambda (var-1682)
                                                             (_let 1
                                                                   (_op 1
                                                                        'cons
                                                                        var-1681
                                                                        var-1682)
                                                                   (lambda (var-1687)
                                                                     (_let 1
                                                                           var-1679
                                                                           (lambda (var-1688)
                                                                             (_let 1
                                                                                   (direct-parse-1335-1373 var-1680
                                                                                                           clone-1669
                                                                                                           clone-1667
                                                                                                           clone-1668
                                                                                                           var-1687
                                                                                                           var-1688)
                                                                                   (lambda (var-1689)
                                                                                     (parse-bar-1339-1471 var-1680
                                                                                                          clone-1669
                                                                                                          clone-1668
                                                                                                          clone-1667
                                                                                                          clone-1666
                                                                                                          var-1689
                                                                                                          var-1678))))))))))))))))))))))
       (lambda ()
         (_let 1
               clone-1670
               (lambda (var-1690)
                 (_let 1
                       clone-1665
                       (lambda (var-1695)
                         (_let 1
                               clone-1664
                               (lambda (var-1696)
                                 (_let 1
                                       clone-1663
                                       (lambda (var-1697)
                                         (let ((var-1698 (cdr clone-1662)))
                                           (loop_2-1355-1651 clone-1669
                                                             clone-1668
                                                             clone-1667
                                                             clone-1666
                                                             var-1698
                                                             var-1690
                                                             var-1695
                                                             var-1696
                                                             var-1697)))))))))))))
(define (loop_5-1344-1432 clone-1440
                          clone-1439
                          clone-1438
                          clone-1437
                          clone-1433
                          clone-1441
                          clone-1436
                          clone-1435
                          clone-1434)
  (if (null? clone-1433)
      (_op 1
           '_sim-error
           (_lift0 1 'direct-parse)
           (_lift0 1 "can't shift on")
           clone-1441)
      (multi-memo 1
                  'loop_5-1345-1442
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-1440
                        clone-1439
                        clone-1438
                        clone-1437
                        clone-1433
                        clone-1441
                        clone-1436
                        clone-1435
                        clone-1434))))
(define (loop_5-1345-1442 clone-1450
                          clone-1449
                          clone-1448
                          clone-1447
                          clone-1443
                          clone-1451
                          clone-1446
                          clone-1445
                          clone-1444)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-1443)) clone-1451)
       (lambda ()
         (_let 1
               clone-1446
               (lambda (var-1452)
                 (let ((var-1457 (car clone-1443)))
                   (_let 1
                         (_op 1 'cdr var-1452)
                         (lambda (var-1458)
                           (_let 1
                                 clone-1445
                                 (lambda (var-1459)
                                   (_let 1
                                         (_op 1 'stream-cdr clone-1444)
                                         (lambda (var-1460)
                                           (let ((var-1461 (goto clone-1447
                                                                 var-1457)))
                                             (_let 1
                                                   var-1458
                                                   (lambda (var-1462)
                                                     (_let 1
                                                           var-1459
                                                           (lambda (var-1463)
                                                             (_let 1
                                                                   (_op 1
                                                                        'cons
                                                                        var-1462
                                                                        var-1463)
                                                                   (lambda (var-1468)
                                                                     (_let 1
                                                                           var-1460
                                                                           (lambda (var-1469)
                                                                             (_let 1
                                                                                   (direct-parse-1335-1373 var-1461
                                                                                                           clone-1450
                                                                                                           clone-1448
                                                                                                           clone-1449
                                                                                                           var-1468
                                                                                                           var-1469)
                                                                                   (lambda (var-1470)
                                                                                     (parse-bar-1339-1471 var-1461
                                                                                                          clone-1450
                                                                                                          clone-1449
                                                                                                          clone-1448
                                                                                                          clone-1447
                                                                                                          var-1470
                                                                                                          var-1459))))))))))))))))))))))
       (lambda ()
         (_let 1
               clone-1451
               (lambda (var-1580)
                 (_let 1
                       clone-1446
                       (lambda (var-1585)
                         (_let 1
                               clone-1445
                               (lambda (var-1586)
                                 (_let 1
                                       clone-1444
                                       (lambda (var-1587)
                                         (let ((var-1588 (cdr clone-1443)))
                                           (loop_5-1344-1432 clone-1450
                                                             clone-1449
                                                             clone-1448
                                                             clone-1447
                                                             var-1588
                                                             var-1580
                                                             var-1585
                                                             var-1586
                                                             var-1587)))))))))))))
(define (parse-bar-1339-1471 clone-1478
                             clone-1476
                             clone-1475
                             clone-1474
                             clone-1473
                             clone-1477
                             clone-1472)
  (if (final? clone-1478 clone-1476)
      clone-1477
      (_let 1
            (_op 1 'result-lhs clone-1477)
            (lambda (var-1479)
              (_let 1
                    (_op 1 'result-dot clone-1477)
                    (lambda (var-1480)
                      (_let 1
                            (_op 1 'result-att clone-1477)
                            (lambda (var-1481)
                              (_let 1
                                    (_op 1 'result-inp clone-1477)
                                    (lambda (var-1482)
                                      (let ((var-1483 (next-nonterminals clone-1473
                                                                         clone-1476)))
                                        (parse-bar-1340-1484 clone-1476
                                                             clone-1475
                                                             clone-1474
                                                             clone-1473
                                                             var-1483
                                                             var-1480
                                                             var-1479
                                                             var-1481
                                                             clone-1472
                                                             var-1482))))))))))))
(define (parse-bar-1340-1484 clone-1492
                             clone-1491
                             clone-1490
                             clone-1489
                             clone-1485
                             clone-1494
                             clone-1493
                             clone-1488
                             clone-1487
                             clone-1486)
  (if (null? clone-1485)
      (_op 1
           'parse-result
           clone-1493
           (_op 1 '- clone-1494 (_lift0 1 1))
           clone-1488
           clone-1486)
      (multi-memo 1
                  'parse-bar-1341-1495
                  '(0 0 0 0 0 1 1 1 1 1)
                  (list clone-1492
                        clone-1491
                        clone-1490
                        clone-1489
                        clone-1485
                        clone-1494
                        clone-1493
                        clone-1488
                        clone-1487
                        clone-1486))))
(define (parse-bar-1341-1495 clone-1503
                             clone-1502
                             clone-1501
                             clone-1500
                             clone-1496
                             clone-1505
                             clone-1504
                             clone-1499
                             clone-1498
                             clone-1497)
  (_if 1
       (_op 1 '< (_lift0 1 1) clone-1505)
       (lambda ()
         (_op 1
              'parse-result
              clone-1504
              (_op 1 '- clone-1505 (_lift0 1 1))
              clone-1499
              clone-1497))
       (lambda ()
         (_let 1
               clone-1504
               (lambda (var-1506)
                 (_let 1
                       clone-1499
                       (lambda (var-1511)
                         (_let 1
                               clone-1498
                               (lambda (var-1512)
                                 (_let 1
                                       clone-1497
                                       (lambda (var-1513)
                                         (loop_6-1342-1515 clone-1503
                                                           clone-1502
                                                           clone-1501
                                                           clone-1500
                                                           clone-1496
                                                           var-1506
                                                           var-1511
                                                           var-1512
                                                           var-1513))))))))))))
(define (loop_6-1342-1515 clone-1523
                          clone-1522
                          clone-1521
                          clone-1520
                          clone-1516
                          clone-1524
                          clone-1519
                          clone-1518
                          clone-1517)
  (if (null? (cdr clone-1516))
      (let ((var-1529 (car clone-1516)))
        (_let 1
              clone-1519
              (lambda (var-1530)
                (_let 1
                      clone-1518
                      (lambda (var-1531)
                        (_let 1
                              clone-1517
                              (lambda (var-1532)
                                (let ((var-1533 (goto clone-1520 var-1529)))
                                  (_let 1
                                        var-1530
                                        (lambda (var-1534)
                                          (_let 1
                                                var-1531
                                                (lambda (var-1535)
                                                  (_let 1
                                                        (_op 1
                                                             'cons
                                                             var-1534
                                                             var-1535)
                                                        (lambda (var-1540)
                                                          (_let 1
                                                                var-1532
                                                                (lambda (var-1541)
                                                                  (_let 1
                                                                        (direct-parse-1335-1373 var-1533
                                                                                                clone-1523
                                                                                                clone-1521
                                                                                                clone-1522
                                                                                                var-1540
                                                                                                var-1541)
                                                                        (lambda (var-1542)
                                                                          (parse-bar-1339-1471 var-1533
                                                                                               clone-1523
                                                                                               clone-1522
                                                                                               clone-1521
                                                                                               clone-1520
                                                                                               var-1542
                                                                                               var-1531)))))))))))))))))))
      (multi-memo 1
                  'loop_6-1343-1543
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-1523
                        clone-1522
                        clone-1521
                        clone-1520
                        clone-1516
                        clone-1524
                        clone-1519
                        clone-1518
                        clone-1517))))
(define (loop_6-1343-1543 clone-1551
                          clone-1550
                          clone-1549
                          clone-1548
                          clone-1544
                          clone-1552
                          clone-1547
                          clone-1546
                          clone-1545)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-1544)) clone-1552)
       (lambda ()
         (let ((var-1557 (car clone-1544)))
           (_let 1
                 clone-1547
                 (lambda (var-1558)
                   (_let 1
                         clone-1546
                         (lambda (var-1559)
                           (_let 1
                                 clone-1545
                                 (lambda (var-1560)
                                   (let ((var-1561 (goto clone-1548 var-1557)))
                                     (_let 1
                                           var-1558
                                           (lambda (var-1562)
                                             (_let 1
                                                   var-1559
                                                   (lambda (var-1563)
                                                     (_let 1
                                                           (_op 1
                                                                'cons
                                                                var-1562
                                                                var-1563)
                                                           (lambda (var-1568)
                                                             (_let 1
                                                                   var-1560
                                                                   (lambda (var-1569)
                                                                     (_let 1
                                                                           (direct-parse-1335-1373 var-1561
                                                                                                   clone-1551
                                                                                                   clone-1549
                                                                                                   clone-1550
                                                                                                   var-1568
                                                                                                   var-1569)
                                                                           (lambda (var-1570)
                                                                             (parse-bar-1339-1471 var-1561
                                                                                                  clone-1551
                                                                                                  clone-1550
                                                                                                  clone-1549
                                                                                                  clone-1548
                                                                                                  var-1570
                                                                                                  var-1559))))))))))))))))))))
       (lambda ()
         (_let 1
               clone-1552
               (lambda (var-1571)
                 (_let 1
                       clone-1547
                       (lambda (var-1576)
                         (_let 1
                               clone-1546
                               (lambda (var-1577)
                                 (_let 1
                                       clone-1545
                                       (lambda (var-1578)
                                         (let ((var-1579 (cdr clone-1544)))
                                           (loop_6-1342-1515 clone-1551
                                                             clone-1550
                                                             clone-1549
                                                             clone-1548
                                                             var-1579
                                                             var-1571
                                                             var-1576
                                                             var-1577
                                                             var-1578)))))))))))))
(define (direct-parse-1337-1387 clone-1390 clone-1389 clone-1388)
  (if (final? clone-1390 clone-1389)
      (let ((var-1393 (static-constructor '1
                                          (lambda ()
                                            (lambda (t_1-1391)
                                              (not (equal? t_1-1391 '$))))
                                          (list)
                                          '())))
        (filter-20-1333-1398 var-1393 clone-1388 '()))
      clone-1388))
(define (filter-20-1333-1398 clone-1401 clone-1400 clone-1399)
  (if (null? clone-1400)
      (reverse clone-1399)
      (let ((var-1407 (cdr clone-1400)))
        (let ((var-1408 (filter-20-1334-1402 clone-1401 clone-1400 clone-1399)))
          (filter-20-1333-1398 clone-1401 var-1407 var-1408)))))
(define (filter-20-1334-1402 clone-1405 clone-1404 clone-1403)
  (if ((clone-1405 'value) (car clone-1404))
      (cons (car clone-1404) clone-1403)
      clone-1403))
(define (direct-parse-1336-1380 clone-1382 clone-1381)
  (_let 1
        (_op 1 'stream-car clone-1381)
        (lambda (var-1383)
          (_if 1
               (_op 1 'equal? (_lift0 1 '$) (_op 1 'car var-1383))
               (lambda ()
                 (_op 1 'car clone-1382))
               (lambda ()
                 (_let 1
                       (_op 1 'stream-car clone-1381)
                       (lambda (var-1384)
                         (_op 1
                              '_sim-error
                              (_lift0 1 'direct-parse)
                              (_lift0 1 "expecting eof")
                              (_op 1 'car var-1384)))))))))
