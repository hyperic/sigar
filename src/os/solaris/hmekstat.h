/*
 * from sys/hme.h, private to _KERNEL.
 * we should be ok provided ksp->ks_data_size == sizeof(struct hmekstat)
 * else will need to fallback to using kstat_data_lookup.
 */

struct	hmekstat {
	struct kstat_named	hk_ipackets;	/* packets received */
	struct kstat_named	hk_ierrors;	/* input errors */
	struct kstat_named	hk_opackets;	/* packets transmitted */
	struct kstat_named	hk_oerrors;	/* output errors */
	struct kstat_named	hk_coll;	/* collisions encountered */
	struct kstat_named	hk_defer;	/* slots deferred */
	struct kstat_named	hk_fram;	/* framing errors */
	struct kstat_named	hk_crc;		/* crc errors */
	struct kstat_named	hk_sqerr;	/* SQE test  errors */
	struct kstat_named	hk_cvc;		/* code violation  errors */
	struct kstat_named	hk_lenerr;	/* rx len errors */
	struct kstat_named	hk_ifspeed;	/* interface speed */
	struct kstat_named	hk_buff;	/* buff errors */
	struct kstat_named	hk_oflo;	/* overflow errors */
	struct kstat_named	hk_uflo;	/* underflow errors */
	struct kstat_named	hk_missed;	/* missed/dropped packets */
	struct kstat_named	hk_tlcol;	/* late collisions */
	struct kstat_named	hk_trtry;	/* retry errors */
	struct kstat_named	hk_fstcol;	/* first collisions */
	struct kstat_named	hk_tnocar;	/* no carrier */
	struct kstat_named	hk_inits;	/* initialization */
	struct kstat_named	hk_nocanput;	/* nocanput errors */
	struct kstat_named	hk_allocbfail;	/* allocb failures */
	struct kstat_named	hk_runt;	/* runt errors */
	struct kstat_named	hk_jab;		/* jabber errors */
	struct kstat_named	hk_babl;	/* runt errors */
	struct kstat_named	hk_tmder;	/* tmd errors */
	struct kstat_named	hk_txlaterr;	/* tx late errors */
	struct kstat_named	hk_rxlaterr;	/* rx late errors */
	struct kstat_named	hk_slvparerr;	/* slave parity errors */
	struct kstat_named	hk_txparerr;	/* tx parity errors */
	struct kstat_named	hk_rxparerr;	/* rx parity errors */
	struct kstat_named	hk_slverrack;	/* slave error acks */
	struct kstat_named	hk_txerrack;	/* tx error acks */
	struct kstat_named	hk_rxerrack;	/* rx error acks */
	struct kstat_named	hk_txtagerr;	/* tx tag error */
	struct kstat_named	hk_rxtagerr;	/* rx tag error */
	struct kstat_named	hk_eoperr;	/* eop error */
	struct kstat_named	hk_notmds;	/* tmd errors */
	struct kstat_named	hk_notbufs;	/* tx buf errors */
	struct kstat_named	hk_norbufs;	/* rx buf errors */
	struct kstat_named	hk_clsn;	/* clsn errors */

	/*
	 * required by kstat for MIB II objects(RFC 1213)
	 */
	struct  kstat_named	hk_rcvbytes; 	/* # octets received */
						/* MIB - ifInOctets */
	struct  kstat_named	hk_xmtbytes; 	/* # octets transmitted */
						/* MIB - ifOutOctets */
	struct  kstat_named	hk_multircv; 	/* # multicast packets */
						/* delivered to upper layer */
						/* MIB - ifInNUcastPkts */
	struct  kstat_named	hk_multixmt; 	/* # multicast packets */
						/* requested to be sent */
						/* MIB - ifOutNUcastPkts */
	struct  kstat_named	hk_brdcstrcv;	/* # broadcast packets */
						/* delivered to upper layer */
						/* MIB - ifInNUcastPkts */
	struct  kstat_named	hk_brdcstxmt;	/* # broadcast packets */
						/* requested to be sent */
						/* MIB - ifOutNUcastPkts */
	struct  kstat_named	hk_norcvbuf; 	/* # rcv packets discarded */
						/* MIB - ifInDiscards */
	struct  kstat_named	hk_noxmtbuf; 	/* # xmt packets discarded */
						/* MIB - ifOutDiscards */

	struct  kstat_named     hk_phyfail;     /* phy failures */
	struct  kstat_named     hk_link_up;     /* Link Status */
};
