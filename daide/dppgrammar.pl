%
% The DPP grammar
% The DPP language is used bi-directionally to communicate between server and the client.
% This API should parse all the incoming messages, and translate the outgoing messages
% such that the reasoning part need not be aware of underlying translations.
% 
% Tokens normally are identified by a three-uppercase-letter name. Our parser shall
% parse these into a corresponding three-lowercase-letter predicate.
% In addition, it should also resolve ambiguities between strings (text tokens)
% and three-lowercase-letter presentations when using Prolog.
%
% Below is a DCG of the language. Refer to dpp_syntax.rtf Appendix 2.
% 
% Test clause:  
% cs_msg(nme(name,'v1.0')) <=>
%  ['NME', '(', txt(n),txt(a), txt(m), txt(e),')','(', txt(v), txt('1'), txt('.'), txt('0'),')'].

% ... Pasted and modified from dpp_bnf.ods...
% this is the official syntax as of 2006.

:-ensure_loaded(dpptokens).

% Client --> Server messages
cs_msg(nme(Name,Version))  --> ['NME'], ['('], string(Name), [')'], ['('], string(Version), [')'].
cs_msg(obs)  --> ['OBS'].
cs_msg(iam(Power,Passcode))  --> ['IAM'], ['('], power(Power),[')'], ['('], number(Passcode), [')'].
cs_msg(map)  --> ['MAP'].
cs_msg(mdf)  --> ['MDF'].
cs_msg(yes(Cmd))  --> ['YES'], ['('], acknowledgable_command(Cmd), [')'].
cs_msg(rej(Cmd))  --> ['REJ'], ['('], acknowledgable_command(Cmd), [')'].
cs_msg(now)  --> ['NOW'].
cs_msg(sco)  --> ['SCO'].
% Note: using 'turn' predicate
cs_msg(hst(turn(Season,Year)))  --> ['HST'], ['('], turn(Season,Year), [')'].
cs_msg(tme(Snds))  --> ['TME'], ['('], number(Snds), [')'].
cs_msg(tme)  --> ['TME'].
cs_msg(adm(Name,Msg))  --> ['ADM'], ['('], string(Name), [')'], ['('], string(Msg), [')'].
cs_msg(prn(Seq))  --> ['PRN'], ['('], bad_bracketed_sequence(Seq), [')'].
cs_msg(huh(Seq))  --> ['HUH'], ['('], any_token_sequence(Seq), [')'].
cs_msg(hlo)  --> ['HLO'].
cs_msg(sub(Orders))  --> ['SUB'], orders(Orders).
cs_msg(not(Ncmd))  --> ['NOT'], ['('], negatable_command(Ncmd), [')'].
cs_msg(mis)  --> ['MIS'].
cs_msg(gof)  --> ['GOF'].
cs_msg(ord)  --> ['ORD'].
cs_msg(drw)  --> ['DRW'].
cs_msg(drw(Power,Powers))  --> ['DRW'], ['('], power(Power), power_list(Powers), [')'].
% cs_msg()  --> 'send_message'. ??
% Server --> Client Messages
sc_msg(yes(Cmd))  --> ['YES'], ['('], client_command(Cmd), [')'].
sc_msg(yes(Req))  --> ['YES'], ['('], client_request(Req), [')'].
sc_msg(rej(Req))  --> ['REJ'], ['('], client_request(Req), [')'].
sc_msg(rej(Cmd))  --> ['REJ'], ['('], rejectable_client_command(Cmd), [')'].
sc_msg(map(Name))  --> ['MAP'], ['('], string(Name), [')'].

% Note: using list as pair here
% Note: using the adjs/2 and adj/2 predicate here.

% Full parse of mdf is:
% mdf( [powers...],
%       [[centres(power,[centre,...]),...],[nonsupplycentre,...]],
%       [has(province,[can_move(unit_type *OR* coast(coast), loc(prov) *OR* loc(prov,coast)),...]),...]
sc_msg(mdf(Powers,[Scs,Nscs], Adjs))  -->
   ['MDF'], ['('], power_list(Powers), [')'],
            ['('], mdf_provinces(Scs,Nscs), [')'],
            ['('], mdf_adjacencies(Adjs), [')'].
% Full parse of hlo is:            
% hlo( power, passcode, [lvl(seconds) *OR* mtl(seconds) *OR* rtl(seconds) *OR* btl(seconds)] ).
sc_msg(hlo(Power,Passcode,Var))  --> ['HLO'], ['('], power(Power), [')'], ['('], number(Passcode), [')'], ['('], variant(Var), [')'].
% Note: using turn/2 predicate
% Full parse of now is:
% now(turn(season,year),[unit(Power,Type,loc(Prov) *OR* loc(Prov,Coast)), ...])
% *OR* (the 'MRT' ['must retreat to' syntax]
% now(turn(season,year),[unit(Power,Type,loc(Prov) *OR* loc(Prov,Coast), mrt([prov,...])), ...])
sc_msg(now(turn(Season,Year),Units))  --> ['NOW'], ['('], turn(Season,Year), [')'], unit_with_location_and_mrts(Units).

% Full parse of sco is:
% sco( [sco_entry(power, [centre, ...]),... ] )
sc_msg(sco(Entries))  --> ['SCO'], sco_entries(Entries). 
sc_msg(thx(Order,Note))  --> ['THX'], ['('], order(Order), [')'], ['('], order_note(Note), [')'].
sc_msg(mis(Units))  --> ['MIS'], unit_with_location_and_mrts(Units).
sc_msg(mis(Disbands))  --> ['MIS'], ['('], number(Disbands), [')'].
% Full parse of ord is:
% ord( 
sc_msg(ord(turn(Season,Year),Order,Res))  --> ['ORD'], ['('], turn(Season,Year), [')'], ['('], order(Order), [')'], ['('], compound_order_result(Res), [')'].
sc_msg(sve(GameName))  --> ['SVE'], ['('], string(GameName), [')'].
sc_msg(lod(GameName))  --> ['LOD'], ['('], string(GameName), [')'].
sc_msg(off)  --> ['OFF'].
sc_msg(tme(Snds))  --> ['TME'], ['('], number(Snds), [')'].
sc_msg(prn)  --> ['PRN'], ['('], bad_bracketed_sequence, [')'].
sc_msg(huh)  --> ['HUH'], ['('], any_token_sequence, [')'].
sc_msg(ccd(Power))  --> ['CCD'], ['('], power(Power), [')'].
sc_msg(not(Msg))  --> ['NOT'], ['('], negated_server_message(Msg), [')'].
sc_msg(adm(Name,Msg))  --> ['ADM'], ['('], string(Name), [')'], ['('], string(Msg), [')'].
sc_msg(slo(Power))  --> ['SLO'], ['('], power(Power), [')'].
sc_msg(drw)  --> ['DRW'].
sc_msg(smr(turn(Season,Year),Summaries))  --> ['SMR'], ['('], turn(Season,Year), [')'], power_summaries(Summaries).
sc_msg(drw(Power,Powers))  --> ['DRW'], ['('], power(Power), power_list(Powers), [')'].
sc_msg(out(Power))  --> ['OUT'], ['('], power(Power), [')'].
sc_msg(frm(Sender,Recipients,Msg))  --> ['FRM'], ['('], power(Sender), [')'], ['('], power_list(Recipients), [')'], ['('], press_message(Msg), [')'].
sc_msg(frm(Sender,Recipients,Rep))  --> ['FRM'], ['('], power(Sender), [')'], ['('], power_list(Recipients), [')'], ['('], reply(Rep), [')'].
acknowledgable_command(map(Name))  --> ['MAP'], ['('], string(Name), [')'].
acknowledgable_command(sve(Name))  --> ['SVE'], ['('], string(Name), [')'].
% any_token_sequence(todo)  --> 'Any sequence of tokens where the brackets all match correctly'.
% bad_bracketed_sequence(todo)  --> 'A sequence of tokens where the bracketing is incorrect'.
coast(Coast)  --> [Coast], { dip_ccv(Coast,0x46, _) }.
compound_order_result(Note)  --> order_note(Note).
compound_order_result(Result)  --> order_result(Result).
compound_order_result(Note)  --> order_note(Note), ['RET'].
compound_order_result(Result)  --> order_result(Result), ['RET'].
client_command(obs)  --> ['OBS'].
client_command(not)  --> ['NOT'], ['('], ['TME'], [')'].
client_request(nme(Name,Version))  --> ['NME'], ['('], string(Name), [')'], ['('], string(Version), [')'].
client_request(iam(Power,Passcode))  --> ['IAM'], ['('], power(Power), [')'], ['('], number(Passcode), [')'].
client_request(not)  --> ['NOT'], ['('], ['GOF'], [')'].
client_request(gof)  --> ['GOF'].
client_request(tme(Seconds))  --> ['TME'], ['('], number(Seconds), [')'].
client_request(drw)  --> ['DRW'].
client_request(not(Req))  --> ['NOT'], ['('], negated_client_request(Req), [')'].
client_request(drw(Power,Powers))  --> ['DRW'], ['('], power(Power), power_list(Powers), [')'].
% client_request(???)  --> 'send_message'.
% Note: using turn predicate
explanation(exp(turn(Season,Year), Rep))  --> ['EXP'], ['('], turn(Season,Year), [')'], ['('], reply(Rep), [')'].
logical_operator(and(Offers))  --> ['AND'], ['('], offers(Offers).
logical_operator(orr(Offers))  --> ['ORR'], ['('], offers(Offers).
mdf_adjacencies(Adjs)  --> mdf_province_adjacenciess(Adjs).
% NOTE: using centres predicate
mdf_centre_list(centres(Power,Provs))  --> power(Power), province_list(Provs).
mdf_centre_list(centres(uno, Provs))  --> ['UNO'], province_list(Provs).
% NOTE: ignoring brackets
mdf_centre_list(Powers,Provs)  --> ['('], power_list(Powers), [')'], province_list(Provs).
mdf_coast('FLT'(Coast))  --> ['FLT'], coast(Coast).

% Note: using coast/1, can_move/2 predicates.
mdf_coast_adjacencies(can_move(Type, Provs))  --> unit_type(Type), mdf_province_list(Provs).
mdf_coast_adjacencies(can_move(FltCoast, Provs))  --> ['('], mdf_coast(FltCoast), [')'], mdf_province_list(Provs).
mdf_coast_adjacenciess([Adj])  -->  ['('], mdf_coast_adjacencies(Adj), [')'].
mdf_coast_adjacenciess([Adj|Tail])  --> ['('], mdf_coast_adjacencies(Adj), [')'], mdf_coast_adjacenciess(Tail).
mdf_provinces(Scs,Nscs)  --> ['('], mdf_supply_centres(Scs), [')'], ['('], province_list(Nscs), [')'].
mdf_province(loc(Prov))  --> province(Prov).
mdf_province(loc(Prov,Coast))  --> ['('], province_and_coast(Prov,Coast), [')'].
mdf_province_adjacencies(has(Prov,Adjs))  --> province(Prov), mdf_coast_adjacenciess(Adjs).
mdf_province_adjacenciess([Adj])  -->  ['('], mdf_province_adjacencies(Adj), [')'].
mdf_province_adjacenciess([Adj|Tail])  --> ['('], mdf_province_adjacencies(Adj), [')'], mdf_province_adjacenciess(Tail).
mdf_province_list([Prov])  --> mdf_province(Prov).
mdf_province_list([Prov|Tail]) --> mdf_province(Prov), mdf_province_list(Tail).
% Note: mdf_province_list can be empty...?
mdf_province_list([])  --> [].
mdf_supply_centre(Cs)  --> ['('], mdf_centre_list(Cs), [')'].
mdf_supply_centres([Sc])  --> mdf_supply_centre(Sc).
mdf_supply_centres([Sc|Tail])  --> mdf_supply_centre(Sc), mdf_supply_centres(Tail).       
negatable_command(sub(Order))  --> ['SUB'], ['('], order(Order), [')'].
negatable_command(sub)  --> ['SUB'].
negatable_command(gof)  --> ['GOF'].
negatable_command(drw)  --> ['DRW'].
negatable_command(drw(Power,Powers))  --> ['DRW'], ['('], power(Power), power_list(Powers), [')'].
negatable_command(tme)  --> ['TME'].
negatable_command(tme)  --> ['TME'], ['('], number, [')'].
negatable_query(qry(Offer))  --> ['QRY'], ['('], offer(Offer), [')'].
negatable_query(not)  --> ['NOT'], ['('], query, [')'].
negated_client_request(tme)  --> ['TME'], ['('], number, [')'].
negated_client_request(drw)  --> ['DRW'].
negated_client_request(drw(Power,Powers))  --> ['DRW'], ['('], power(Power), power_list(Powers), [')'].
negated_server_message(ccd(Power))  --> ['CCD'], ['('], power(Power), [')'].
negated_server_message(tme(Number))  --> ['TME'], ['('], number(Number), [')'].
number(X)  --> [X], { integer(X), -8192 =< X, X < 8192 }.
offer(pce(Powers))  --> ['PCE'], ['('], power_list(Powers), [')'].
offer(aly(Allies,Enemies))  --> ['ALY'], ['('], power_list(Allies), [')'], 'VSS', ['('], power_list(Enemies), [')'].
offer(drw)  --> ['DRW'].
offer(slo(Power))  --> ['SLO'], ['('], power(Power), [')'].
offer(not(Offer))  --> ['NOT'], ['('], offer(Offer), [')'].
offer(xdo(Order))  --> ['XDO'], ['('], order(Order), [')'].
offer(dmz(Powers,Provs))  --> ['DMZ'], ['('], power_list(Powers), [')'], ['('], province_list(Provs), [')'].
offer(scd(OwnLists))  --> ['SCD'], sc_ownership_lists(OwnLists).
offer(occ(Units))  --> ['OCC'], occ_unit_with_locations(Units).
offer(and(Offers))  --> ['AND'], offers(Offers).
offer(orr(Offers))  --> ['ORR'], offers(Offers).
offer(cho(Min,Max,Offers))  --> ['CHO'], ['('], number(Min), number(Max), [')'], offers(Offers).
offer(for(turn(Season,Year),Offer))  --> ['FOR'], ['('], turn(Season,Year), [')'], ['('], offer(Offer), [')'].
offer(for(Period,Offer))  --> ['FOR'], ['('], period(Period), [')'], ['('], offer(Offer), [')'].
offer(xoy(Power1,Power2))  --> ['XOY'], ['('], power(Power1), [')'], ['('], power(Power2), [')'].
offer(ydo(Power,Units))  --> ['YDO'], ['('], power(Power), [')'], unit_with_locations(Units).
offer(snd(Sender,Recipients,Msg))  --> ['SND'], ['('], power(Sender), [')'], ['('], power_list(Recipients), [')'], ['('], press_message(Msg), [')'].
offer(snd(Sender,Recipients,Rep))  --> ['SND'], ['('], power(Sender), [')'], ['('], power_list(Recipients), [')'], ['('], reply(Rep), [')'].
offer(fwd(Senders,Recipient,Forwarders))  --> ['FWD'], ['('], power_list(Senders), [')'], ['('], power(Recipient), [')'], ['('], power(Forwarders), [')'].
offer(bcc(Sender,Forwarders,Recipient))  --> ['BCC'], ['('], power(Sender), [')'], ['('], power_list(Forwarders), [')'], ['('], power(Recipient), [')'].
offers([Offer])  --> ['('], offer(Offer), [')'].
offers([Offer|Tail])  --> ['('], offer(Offer), ['('], offers(Tail).
% Note using 'order' predicate
% Note: using infix operator as prefix. Add new operators!
order(hld(Unit))  --> ['('], unit_with_location(Unit), [')'], ['HLD'].
order(mto(Unit,Prov))  --> ['('], unit_with_location(Unit), [')'], ['MTO'], mdf_province(Prov).
order(sup(SUnit,Unit))  --> ['('], unit_with_location(SUnit), [')'], ['SUP'], ['('], unit_with_location(Unit), [')'].
order(sup(SUnit,mto(Unit,Prov)))  --> ['('], unit_with_location(SUnit), [')'], ['SUP'], ['('], unit_with_location(Unit), [')'], ['MTO'], province(Prov).
order(cvy(CUnit,cto(Unit,Prov)))  --> ['('], unit_with_location(CUnit), [')'], ['CVY'], ['('], unit_with_location(Unit), [')'], ['CTO'], province(Prov).
order(cto(Unit,Prov,via(Provs)))  --> ['('], unit_with_location(Unit), [')'], ['CTO'], province(Prov), ['VIA'], ['('], province_list(Provs), [')'].
order(rto(Unit,Prov))  --> ['('], unit_with_location(Unit), [')'], ['RTO'], mdf_province(Prov).
order(dsb(Unit))  --> ['('], unit_with_location(Unit), [')'], ['DSB'].
order(bld(Unit))  --> ['('], unit_with_location(Unit), [')'], ['BLD'].
order(rem(Unit))  --> ['('], unit_with_location(Unit), [')'], ['REM'].
order(wve(Power))  --> power(Power), ['WVE'].
orders([Order]) --> ['('], order(Order), [')'].
orders([Order|Tail])  --> ['('], order(Order), [')'], orders(Tail).
order_note(mbv)  --> ['MBV'].
order_note(far)  --> ['FAR'].
order_note(nsp)  --> ['NSP'].
order_note(nsu)  --> ['NSU'].
order_note(nas)  --> ['NAS'].
order_note(nsf)  --> ['NSF'].
order_note(nsa)  --> ['NSA'].
order_note(nyu)  --> ['NYU'].
order_note(nrn)  --> ['NRN'].
order_note(nvr)  --> ['NVR'].
order_note(ysc)  --> ['YSC'].
order_note(esc)  --> ['ESC'].
order_note(hsc)  --> ['HSC'].
order_note(nsc)  --> ['NSC'].
order_note(cst)  --> ['CST'].
order_note(nmb)  --> ['NMB'].
order_note(nmr)  --> ['NMR'].
order_note(nrs)  --> ['NRS'].
order_result(suc)  --> ['SUC'].
order_result(bnc)  --> ['BNC'].
order_result(cut)  --> ['CUT'].
order_result(dsr)  --> ['DSR'].
order_result(nso)  --> ['NSO'].
period(todo)  --> ['('], turn(Season,Year), [')'], ['('], turn(Season,Year), [')'].
power(Power)  --> [Power], { dip_ccv(Power, 0x41, _) }.
powers([Power]) --> power(Power).
powers([Power|Tail]) --> power(Power), powers(Tail). 
power_list(List)  --> powers(List).
% Note: uses psum/4/5 predicate.
power_summary(psum(Power,Name, Version, Scs))  --> power(Power), ['('], string(Name), [')'], ['('], string(Version), [')'], number(Scs).
power_summary(psum(Power,Name, Version, 0, YearOfElimination))  --> power(Power), ['('], string(Name), [')'], ['('], string(Version), [')'], number(0), number(YearOfElimination).
power_summaries([Psum])         --> ['('], power_summary(Psum), [')'].
power_summaries([Psum|Tail])  --> ['('], power_summary(Psum), [')'], power_summaries(Tail).
press_message(prp(Offer))  --> ['PRP'], ['('], offer(Offer), [')'].
press_message(try(Params))  --> ['TRY'], ['('], try_parameters(Params), [')'].
press_message(prp(Op))  --> ['PRP'], ['('], logical_operator(Op), [')'].
press_message(ins(Offer))  --> ['INS'], ['('], offer(Offer), [')'].
press_message(qry(Offer))  --> ['QRY'], ['('], offer(Offer), [')'].
press_message(sug(Offer))  --> ['SUG'], ['('], offer(Offer), [')'].
press_message(thk(Offer))  --> ['THK'], ['('], offer(Offer), [')'].
press_message(fct(Offer))  --> ['FCT'], ['('], offer(Offer), [')'].
press_message(wht(Unit))  --> ['WHT'], ['('], unit_with_location(Unit), [')'].
press_message(how(Prov))  --> ['HOW'], ['('], province(Prov), [')'].
press_message(how(Power))  --> ['HOW'], ['('], power(Power), [')'].
press_message(exp(turn(Season,Year),Rep))  --> ['EXP'], ['('], turn(Season,Year), [')'], ['('], reply(Rep), [')'].
press_message(iff(Offer,ThenMsg))  -->
   ['IFF'], ['('], offer(Offer), [')'],
   ['THN'], ['('], press_message(ThenMsg), [')'].
press_message(iff(Offer,ThenMsg,ElseMsg))  -->
   ['IFF'], ['('], offer(Offer), [')'],
   ['THN'], ['('], press_message(ThenMsg), [')'],
   ['ELS'], ['('], press_message(ElseMsg), [')'].
press_message(frm(Sender,Recipients,Msg))  --> ['FRM'], ['('], power(Sender), [')'], ['('], power_list(Recipients), [')'], ['('], press_message(Msg), [')'].
press_message(frm(Sender,Recipients,Rep))  --> ['FRM'], ['('], power(Sender), [')'], ['('], power_list(Recipients), [')'], ['('], reply(Rep), [')'].
press_message(Msg)  --> string(Msg).
province(Prov)  --> [Prov], { dip_ccv(Prov,0x50,_);
                              dip_ccv(Prov,0x51,_);
                              dip_ccv(Prov,0x52,_);
                              dip_ccv(Prov,0x53,_);
                              dip_ccv(Prov,0x54,_);
                              dip_ccv(Prov,0x55,_);
                              dip_ccv(Prov,0x56,_);
                              dip_ccv(Prov,0x57,_) }.
provinces([Prov])  --> province(Prov).
provinces([Prov|Tail])  --> province(Prov), provinces(Tail).
province_list(Provs)  --> provinces(Provs).
province_and_coast(Prov,Coast)  --> province(Prov), coast(Coast).
query(qry(Offer))  --> ['QRY'], ['('], offer(Offer), [')'].
reply(yes(Msg))  --> ['YES'], ['('], press_message(Msg), [')'].
reply(rej(Msg))  --> ['REJ'], ['('], press_message(Msg), [')'].
reply(bwx(Msg))  --> ['BWX'], ['('], press_message(Msg), [')'].
reply(huh(Seq))  --> ['HUH'], ['('], any_token_sequence(Seq), [')'].
reply(thk(Qry))  --> ['THK'], ['('], negatable_query(Qry), [')'].
reply(fct(Qry))  --> ['FCT'], ['('], negatable_query(Qry), [')'].
reply(idk(Qry))  --> ['IDK'], ['('], query(Qry), [')'].
reply(yes(Exp))  --> ['YES'], ['('], explanation(Exp), [')'].
reply(rej(Exp))  --> ['REJ'], ['('], explanation(Exp), [')'].
reply(idk(Exp))  --> ['IDK'], ['('], explanation(Exp), [')'].
reply(sry(Exp))  --> ['SRY'], ['('], explanation(Exp), [')'].
reply(why(Taf))  --> ['WHY'], ['('], think_and_fact(Taf), [')'].
reply(pob(Seq))  --> ['POB'], ['('],why_sequence(Seq), [')'].
reply(why(sug(Offer)))  --> ['WHY'], ['('], 'SUG', ['('], offer(Offer), [')'], [')'].
reply(why(prp(Offer)))  --> ['WHY'], ['('], 'PRP', ['('], offer(Offer), [')'], [')'].
reply(why(ins(Offer)))  --> ['WHY'], ['('], 'INS', ['('], offer(Offer), [')'], [')'].
reply(idk(prp(Offer)))  --> ['IDK'], ['('], 'PRP', ['('], offer(Offer), [')'], [')'].
reply(idk(ins(Offer)))  --> ['IDK'], ['('], 'INS', ['('], offer(Offer), [')'], [')'].
reply(idk(sug(Offer)))  --> ['IDK'], ['('], 'SUG', ['('], offer(Offer), [')'], [')'].
reply(Msg)  --> press_message(Msg).
rejectable_client_command(hlo)  --> ['HLO'].
rejectable_client_command(now)  --> ['NOW'].
rejectable_client_command(sco)  --> ['SCO'].
rejectable_client_command(hst(Season,Year))  --> ['HST'], ['('], turn(Season,Year), [')'].
rejectable_client_command(sub(Orders))  --> ['SUB'], orders(Orders).
rejectable_client_command(ord)  --> ['ORD'].
rejectable_client_command(tme)  --> ['TME'].
rejectable_client_command(adm)  --> ['ADM'], ['('], string(Str), [')'], ['('], string(Str), [')'].
sc_ownership_list(Power,Provs)  --> power(Power), province_list(Provs).
sc_ownership_list(uno(Provs))  --> ['UNO'], province_list(Provs).
sc_ownership_lists([List])  --> ['('], sc_ownership_list(List), [')'].
sc_ownership_lists([List|Tail])  --> ['('], sc_ownership_list(List), [')'], sc_ownership_lists(Tail).
% NOTE: using predicate 'sco_entry'
% NOTE: matching against unknown power ['UNO'].
sco_entry(sco_entry(Power,Provs))  --> power(Power), province_list(Provs).
sco_entry(sco_entry('UNO', Provs))  --> ['UNO'], province_list(Provs).
sco_entries([Entry])  --> ['('], sco_entry(Entry), [')'].
sco_entries([Entry|Tail])  --> ['('], sco_entry(Entry), [')'], sco_entries(Tail).
season(Season)  --> [Season], { dip_ccv(Season,0x47,_) }.
send_message(snd(Powers,Msg))  --> ['SND'], ['('], power_list(Powers), [')'], ['('], press_message(Msg), [')'].
send_message(snd(Powers,Msg))  --> ['SND'], ['('], power_list(Powers), [')'], ['('], reply(Msg), [')'].

% 'strings' are actually just atoms. Therefore, the head H in atom_chars(Str,[H|T])
% will unify with a character.
% Strings may not be empty.
char(Char)  --> [txt(Char)], { atom_length(Char,1) }.
string(Char) --> char(Char).
string(Str)  --> { nonvar(Str), atom_chars(Str,[Char|T]) }, char(Char), { atom_chars(Substr,T) }, string(Substr).
string(Str)  --> { var(Str) }, char(Char), string(Substr), { atom_concat(Char, Substr, Str) }.

think_and_fact(thk(Offer))  --> ['THK'], ['('], offer(Offer), [')'].
think_and_fact(fct(Offer))  --> ['FCT'], ['('], offer(Offer), [')'].
try_parameters(Tokens)  --> try_tokens(Tokens).
try_tokens([Try]) --> try_token(Try). 
try_tokens([Try|Tail]) --> try_token(Try), try_tokens(Tail).
try_token(Try) --> try_token_cs(Try).
try_token(Try) --> try_token_sc(Try).
try_token_cs(prp)  --> ['PRP'].
try_token_cs(pce)  --> ['PCE'].
try_token_cs(aly)  --> ['ALY'].
try_token_cs(vss)  --> ['VSS'].
try_token_cs(drw)  --> ['DRW'].
try_token_cs(slo)  --> ['SLO'].
try_token_cs(not)  --> ['NOT'].
try_token_cs(yes)  --> ['YES'].
try_token_cs(rej)  --> ['REJ'].
try_token_cs(bwx)  --> ['BWX'].
try_token_cs(xdo)  --> ['XDO'].
try_token_cs(dmz)  --> ['DMZ'].
try_token_cs(and)  --> ['AND'].
try_token_cs(orr)  --> ['ORR'].
try_token_cs(scd)  --> ['SCD'].
try_token_cs(occ)  --> ['OCC'].
try_token_cs(ins)  --> ['INS'].
try_token_cs(qry)  --> ['QRY'].
try_token_cs(thk)  --> ['THK'].
try_token_cs(fct)  --> ['FCT'].
try_token_cs(idk)  --> ['IDK'].
try_token_cs(sug)  --> ['SUG'].
try_token_cs(wht)  --> ['WHT'].
try_token_cs(how)  --> ['HOW'].
try_token_cs(exp)  --> ['EXP'].
try_token_cs(sry)  --> ['SRY'].
try_token_cs(for)  --> ['FOR'].
try_token_cs(iff)  --> ['IFF'].
try_token_cs(thn)  --> ['THN'].
try_token_cs(els)  --> ['ELS'].
try_token_cs(xoy)  --> ['XOY'].
try_token_cs(ydo)  --> ['YDO'].
try_token_cs(frm)  --> ['FRM'].
try_token_cs(fwd)  --> ['FWD'].
try_token_cs(snd)  --> ['SND'].
try_token_cs(why)  --> ['WHY'].
try_token_cs(pob)  --> ['POB'].
try_token_sc(prp)  --> ['PRP'].
try_token_sc(pce)  --> ['PCE'].
try_token_sc(aly)  --> ['ALY'].
try_token_sc(vss)  --> ['VSS'].
try_token_sc(drw)  --> ['DRW'].
try_token_sc(slo)  --> ['SLO'].
try_token_sc(not)  --> ['NOT'].
try_token_sc(yes)  --> ['YES'].
try_token_sc(rej)  --> ['REJ'].
try_token_sc(bwx)  --> ['BWX'].
try_token_sc(xdo)  --> ['XDO'].
try_token_sc(dmz)  --> ['DMZ'].
try_token_sc(and)  --> ['AND'].
try_token_sc(orr)  --> ['ORR'].
try_token_sc(scd)  --> ['SCD'].
try_token_sc(occ)  --> ['OCC'].
try_token_sc(ins)  --> ['INS'].
try_token_sc(qry)  --> ['QRY'].
try_token_sc(thk)  --> ['THK'].
try_token_sc(fct)  --> ['FCT'].
try_token_sc(idk)  --> ['IDK'].
try_token_sc(sug)  --> ['SUG'].
try_token_sc(wht)  --> ['WHT'].
try_token_sc(how)  --> ['HOW'].
try_token_sc(exp)  --> ['EXP'].
try_token_sc(sry)  --> ['SRY'].
try_token_sc(for)  --> ['FOR'].
try_token_sc(iff)  --> ['IFF'].
try_token_sc(thn)  --> ['THN'].
try_token_sc(els)  --> ['ELS'].
try_token_sc(xoy)  --> ['XOY'].
try_token_sc(ydo)  --> ['YDO'].
try_token_sc(frm)  --> ['FRM'].
try_token_sc(fwd)  --> ['FWD'].
try_token_sc(snd)  --> ['SND'].
try_token_sc(why)  --> ['WHY'].
try_token_sc(pob)  --> ['POB'].
%%%%
turn(Season,Year)  --> season(Season), number(Year).
unit_type(Type)  --> [Type], { dip_ccv(Type,0x42,_) }.
% note: using 'loc' and 'unit' predicate:
% loc/1 and loc/2 represent province, and province_and_coast respectively.
unit_with_location(unit(Power,Type,loc(Prov)))  --> power(Power), unit_type(Type), province(Prov).
unit_with_location(unit(Power,Type,loc(Prov,Coast)))  --> power(Power), unit_type(Type), ['('], province_and_coast(Prov,Coast), [')'].
unit_with_locations([Unit])  --> ['('], unit_with_location(Unit), [')'].
unit_with_locations([Unit|Tail])  --> ['('], unit_with_location(Unit), [')'], unit_with_locations(Tail).
occ_unit_with_location(Power,Type,loc(Prov))  --> power(Power), unit_type(Type), province(Prov).
occ_unit_with_location(Power,Type,loc(Prov,Coast))  --> power(Power), unit_type(Type), ['('], province_and_coast(Prov,Coast), [')'].
% Note: infix
occ_unit_with_location(unt(Power,Prov))  --> power(Power), ['UNT'], province(Prov).
occ_unit_with_locations([Unit])  --> occ_unit_with_location(Unit).
occ_unit_with_locations([Unit|Tail])  --> occ_unit_with_location(Unit), occ_unit_with_locations(Tail).
% note: using 'loc' and 'unit' predicate.
unit_with_location_and_mrt(unit(Power,Type,loc(Prov)))  --> power(Power), unit_type(Type), province(Prov).
unit_with_location_and_mrt(unit(Power,Type,loc(Prov,Coast)))  --> power(Power), unit_type(Type), ['('], province_and_coast(Prov,Coast), [')'].
unit_with_location_and_mrt(unit(Power,Type,loc(Prov),mrt(Provs)))  --> power(Power), unit_type(Type), province(Prov), ['MRT'], ['('], mdf_province_list(Provs), [')'].
unit_with_location_and_mrt(unit(Power,Type,loc(Prov,Coast),mrt(Provs)))  --> power(Power), unit_type(Type), ['('], province_and_coast(Prov,Coast), [')'], ['MRT'], ['('], mdf_province_list(Provs), [')'].
unit_with_location_and_mrts([Unit]) --> ['('], unit_with_location_and_mrt(Unit), [')'].
unit_with_location_and_mrts([Unit|Tail]) --> ['('], unit_with_location_and_mrt(Unit), [')'], unit_with_location_and_mrts(Tail).
% note: variant is a list of variant options, which can be of the types below...
variant(Options)  --> variant_options(Options).
variant_option(lvl(SyntaxLevel))  --> ['LVL'], number(SyntaxLevel), {!}.
variant_option(mtl(Secs))  --> ['MTL'], number(Secs), {!}.
variant_option(rtl(Secs))  --> ['RTL'], number(Secs), {!}.
variant_option(btl(Secs))  --> ['BTL'], number(Secs), {!}.
variant_option(aoa)  --> ['AOA'], {!}.
variant_option(dsd)  --> ['DSD'], {!}.
variant_option(pda)  --> ['PDA'], {!}.
variant_option(ptl(Secs))  --> ['PTL'], number(Secs), {!}.
variant_option(npr)  --> ['NPR'], {!}.
variant_option(npb)  --> ['NPB'], {!}.
variant_options([Option]) --> ['('], variant_option(Option), [')'].
variant_options([Option|Tail]) --> ['('], variant_option(Option), [')'], variant_options(Tail), {!}.
why_sequence(why(Taf))  --> ['WHY'], ['('], think_and_fact(Taf), [')'].
