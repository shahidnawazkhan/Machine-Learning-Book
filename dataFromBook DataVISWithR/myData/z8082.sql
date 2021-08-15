# ************************************************************
# Sequel Pro SQL dump
# Version 4096
#
# http://www.sequelpro.com/
# http://code.google.com/p/sequel-pro/
#
# Host: localhost (MySQL 5.1.44)
# Datenbank: datendesign
# Erstellungsdauer: 2014-01-31 19:15:32 +0000
# ************************************************************


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;


# Export von Tabelle z8082
# ------------------------------------------------------------

DROP TABLE IF EXISTS `z8082`;

CREATE TABLE `z8082` (
  `jahrc` varchar(200) NOT NULL DEFAULT '',
  `jahr` int(11) DEFAULT NULL,
  `England_Weizen` int(11) DEFAULT NULL,
  `Belgien_Weizen` int(11) DEFAULT NULL,
  `Frankreich_Weizen` int(11) DEFAULT NULL,
  `Oberitalien_Weizen` int(11) DEFAULT NULL,
  `Niederlande_Roggen` int(11) DEFAULT NULL,
  `Deutschland_Roggen` int(11) DEFAULT NULL,
  `Oesterreich_Roggen` int(11) DEFAULT NULL,
  `Polen_Roggen` int(11) DEFAULT NULL,
  `Min_Getreide` int(11) DEFAULT NULL,
  `Max_Getreide` int(11) DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

LOCK TABLES `z8082` WRITE;
/*!40000 ALTER TABLE `z8082` DISABLE KEYS */;

INSERT INTO `z8082` (`jahrc`, `jahr`, `England_Weizen`, `Belgien_Weizen`, `Frankreich_Weizen`, `Oberitalien_Weizen`, `Niederlande_Roggen`, `Deutschland_Roggen`, `Oesterreich_Roggen`, `Polen_Roggen`, `Min_Getreide`, `Max_Getreide`)
VALUES
	('1201-1210',1201,NULL,NULL,19,21,NULL,NULL,NULL,NULL,19,21),
	('1211-1220',1211,26,NULL,21,23,NULL,NULL,NULL,NULL,21,26),
	('1221-1230',1221,35,NULL,25,NULL,NULL,NULL,NULL,NULL,25,35),
	('1231-1240',1231,28,NULL,28,18,NULL,NULL,NULL,NULL,28,28),
	('1241-1250',1241,28,NULL,37,27,NULL,NULL,NULL,NULL,28,37),
	('1251-1260',1251,38,NULL,23,33,NULL,NULL,NULL,NULL,23,38),
	('1261-1270',1261,35,NULL,28,26,NULL,NULL,NULL,NULL,28,35),
	('1271-1280',1271,42,NULL,27,45,NULL,NULL,NULL,NULL,27,45),
	('1281-1290',1281,37,NULL,30,53,NULL,NULL,NULL,NULL,30,53),
	('1291-1300',1291,45,NULL,52,59,NULL,NULL,NULL,NULL,45,59),
	('1301-1310',1301,41,NULL,41,49,NULL,NULL,NULL,NULL,41,49),
	('1311-1320',1311,58,NULL,66,53,NULL,NULL,NULL,NULL,53,66),
	('1321-1330',1321,50,NULL,43,80,NULL,NULL,NULL,NULL,43,80),
	('1331-1340',1331,34,NULL,29,67,NULL,NULL,NULL,NULL,29,67),
	('1341-1350',1341,35,NULL,59,69,NULL,16,NULL,NULL,16,69),
	('1351-1360',1351,38,NULL,52,63,NULL,21,NULL,NULL,21,63),
	('1361-1370',1361,46,NULL,54,66,NULL,28,NULL,3,3,66),
	('1371-1380',1371,36,NULL,45,72,NULL,21,NULL,6,6,72),
	('1381-1390',1381,30,NULL,22,NULL,NULL,20,NULL,3,3,30),
	('1391-1400',1391,30,NULL,30,NULL,NULL,20,NULL,3,3,30),
	('1401-1410',1401,35,27,25,NULL,NULL,24,NULL,5,5,35),
	('1411-1420',1411,28,28,38,NULL,NULL,19,NULL,5,5,38),
	('1421-1430',1421,26,31,59,NULL,NULL,18,NULL,3,3,59),
	('1431-1440',1431,34,47,50,NULL,NULL,21,18,NULL,21,50),
	('1441-1450',1441,25,34,21,NULL,NULL,16,13,NULL,13,34),
	('1451-1460',1451,27,35,21,NULL,NULL,19,8,NULL,8,35),
	('1461-1470',1461,21,23,16,NULL,15,14,19,NULL,19,23),
	('1471-1480',1471,21,26,18,NULL,29,15,12,NULL,12,29),
	('1481-1490',1481,25,36,26,NULL,31,21,13,3,3,36),
	('1491-1500',1491,20,26,17,NULL,21,23,18,1,1,26),
	('1501-1510',1501,21,24,28,51,20,20,14,3,3,51),
	('1511-1520',1511,26,28,29,49,23,17,13,3,3,49),
	('1521-1530',1521,28,37,48,80,43,19,13,4,4,80),
	('1531-1540',1531,27,41,48,72,31,26,17,2,2,72),
	('1541-1550',1541,20,48,50,55,31,25,15,5,5,55),
	('1551-1560',1551,35,52,54,90,42,31,17,9,9,90),
	('1561-1570',1561,33,60,81,90,43,39,34,8,8,90),
	('1571-1580',1571,47,79,98,102,70,51,44,12,12,102),
	('1581-1590',1581,63,123,141,110,52,48,35,8,8,141),
	('1591-1600',1591,91,92,187,157,65,52,38,12,12,187),
	('1601-1610',1601,83,79,91,123,49,45,40,27,27,123),
	('1611-1620',1611,93,71,96,106,51,52,47,19,19,106),
	('1621-1630',1621,107,105,124,151,72,76,40,26,26,151),
	('1631-1640',1631,109,118,123,109,73,84,46,17,17,123),
	('1641-1650',1641,122,120,116,105,67,54,47,17,17,122),
	('1651-1660',1651,104,103,118,73,72,40,34,23,23,118),
	('1661-1670',1661,103,90,104,64,67,40,44,17,17,104),
	('1671-1680',1671,107,97,80,79,58,42,29,15,15,107),
	('1681-1690',1681,83,76,71,57,44,35,36,7,7,83),
	('1691-1700',1691,124,115,102,63,75,66,50,14,14,124),
	('1701-1710',1701,91,91,85,67,58,44,38,9,9,91),
	('1711-1720',1711,98,73,72,59,51,51,46,10,10,98),
	('1721-1730',1721,91,58,66,46,45,42,35,4,4,91),
	('1730-1740',1730,82,62,70,63,44,45,36,11,11,82),
	('1741-1750',1741,74,66,60,69,50,52,43,11,11,74),
	('1751-1760',1751,95,60,68,67,49,51,36,9,9,95),
	('1761-1770',1761,109,69,74,70,54,58,39,12,12,109),
	('1771-1780',1771,113,72,86,87,65,62,48,13,13,113),
	('1781-1790',1781,118,82,93,93,69,57,43,13,13,118),
	('1791-1800',1791,157,NULL,106,116,88,75,47,14,14,157),
	('1801-1810',1801,208,NULL,115,130,117,95,94,NULL,94,208),
	('1811-1820',1811,216,NULL,143,138,103,96,84,NULL,84,216),
	('1821-1830',1821,147,NULL,107,86,56,51,49,NULL,49,147),
	('1831-1840',1831,141,NULL,110,93,71,62,49,NULL,49,141),
	('1841-1850',1841,132,NULL,111,107,81,72,63,NULL,63,132),
	('1851-1860',1851,135,NULL,123,123,98,84,80,NULL,80,135),
	('1861-1870',1861,126,NULL,125,115,90,85,76,NULL,76,126),
	('1871-1880',1871,126,NULL,133,NULL,91,95,101,NULL,91,133),
	('1881-1890',1881,88,NULL,107,NULL,76,85,93,NULL,76,107),
	('1891-1900',1891,69,NULL,96,NULL,63,82,94,NULL,63,96),
	('1901-1910',1901,74,NULL,100,NULL,NULL,87,104,NULL,74,104),
	('1911-1920',1911,89,NULL,90,NULL,NULL,105,123,NULL,89,123),
	('1921-1930',1921,119,NULL,138,NULL,NULL,112,101,NULL,101,138),
	('1931-1940',1931,64,NULL,87,NULL,NULL,99,64,NULL,64,99),
	('1941-1950',1941,135,NULL,159,NULL,NULL,116,112,NULL,112,159),
	('1951-1960',1951,187,NULL,216,NULL,NULL,216,210,NULL,187,216);

/*!40000 ALTER TABLE `z8082` ENABLE KEYS */;
UNLOCK TABLES;



/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
