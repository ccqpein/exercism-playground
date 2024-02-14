use std::collections::HashMap;

pub struct CodonsInfo<'a> {
    // We fake using 'a here, so the compiler does not complain that
    // "parameter `'a` is never used". Delete when no longer needed.
    //phantom: std::marker::PhantomData<&'a ()>,
    table: HashMap<&'a str, &'a str>,
}

impl<'a> CodonsInfo<'a> {
    pub fn name_for(&self, codon: &str) -> Option<&'a str> {
        self.table.get(codon).cloned()
    }

    pub fn of_rna(&self, rna: &str) -> Option<Vec<&'a str>> {
        let a = rna.chars().collect::<Vec<_>>();
        let a = a.chunks(3).map(|cc| String::from_iter(cc));
        //.collect();
        let mut result = vec![];
        for co in a {
            match self.name_for(&co) {
                Some(x) if x == "stop codon" => return Some(result),
                Some(y) => result.push(y),
                None => return None,
            }
        }
        Some(result)
    }
}

pub fn parse<'a>(pairs: Vec<(&'a str, &'a str)>) -> CodonsInfo<'a> {
    CodonsInfo {
        table: pairs.into_iter().collect(),
    }
}

#[test]
fn methionine() {
    let info = parse(make_pairs());
    assert_eq!(info.name_for("AUG"), Some("methionine"));
}

#[test]
fn cysteine_tgt() {
    let info = parse(make_pairs());
    assert_eq!(info.name_for("UGU"), Some("cysteine"));
}

#[test]
fn stop() {
    let info = parse(make_pairs());
    assert_eq!(info.name_for("UAA"), Some("stop codon"));
}

#[test]
fn valine() {
    let info = parse(make_pairs());
    assert_eq!(info.name_for("GUU"), Some("valine"));
}

#[test]
fn isoleucine() {
    let info = parse(make_pairs());
    assert_eq!(info.name_for("AUU"), Some("isoleucine"));
}

#[test]
fn arginine_name() {
    let info = parse(make_pairs());
    assert_eq!(info.name_for("CGA"), Some("arginine"));
    assert_eq!(info.name_for("AGA"), Some("arginine"));
    assert_eq!(info.name_for("AGG"), Some("arginine"));
}

#[test]
fn empty_is_invalid() {
    let info = parse(make_pairs());
    assert!(info.name_for("").is_none());
}

#[test]
fn x_is_not_shorthand_so_is_invalid() {
    let info = parse(make_pairs());
    assert!(info.name_for("VWX").is_none());
}

#[test]
fn too_short_is_invalid() {
    let info = parse(make_pairs());
    assert!(info.name_for("AU").is_none());
}

#[test]
fn too_long_is_invalid() {
    let info = parse(make_pairs());
    assert!(info.name_for("ATTA").is_none());
}

#[test]
fn translates_rna_strand_into_correct_protein() {
    let info = parse(make_pairs());
    assert_eq!(
        info.of_rna("AUGUUUUGG"),
        Some(vec!["methionine", "phenylalanine", "tryptophan"])
    );
}

#[test]
fn stops_translation_if_stop_codon_present() {
    let info = parse(make_pairs());
    assert_eq!(
        info.of_rna("AUGUUUUAA"),
        Some(vec!["methionine", "phenylalanine"])
    );
}

#[test]
fn stops_translation_of_longer_strand() {
    let info = parse(make_pairs());
    assert_eq!(
        info.of_rna("UGGUGUUAUUAAUGGUUU"),
        Some(vec!["tryptophan", "cysteine", "tyrosine"])
    );
}

#[test]
fn invalid_codons() {
    let info = parse(make_pairs());
    assert!(info.of_rna("CARROT").is_none());
}

#[test]
fn invalid_length() {
    let info = parse(make_pairs());
    assert!(info.of_rna("AUGUA").is_none());
}

#[test]
fn valid_stopped_rna() {
    let info = parse(make_pairs());
    assert_eq!(info.of_rna("AUGUAAASDF"), Some(vec!["methionine"]));
}

// The input data constructor. Returns a list of codon, name pairs.
fn make_pairs() -> Vec<(&'static str, &'static str)> {
    let grouped = vec![
        ("isoleucine", vec!["AUU", "AUC", "AUA"]),
        ("valine", vec!["GUU", "GUC", "GUA", "GUG"]),
        ("phenylalanine", vec!["UUU", "UUC"]),
        ("methionine", vec!["AUG"]),
        ("cysteine", vec!["UGU", "UGC"]),
        ("alanine", vec!["GCU", "GCC", "GCA", "GCG"]),
        ("glycine", vec!["GGU", "GGC", "GGA", "GGG"]),
        ("proline", vec!["CCU", "CCC", "CCA", "CCG"]),
        ("threonine", vec!["ACU", "ACC", "ACA", "ACG"]),
        ("serine", vec!["AGU", "AGC"]),
        ("tyrosine", vec!["UAU", "UAC"]),
        ("tryptophan", vec!["UGG"]),
        ("glutamine", vec!["CAA", "CAG"]),
        ("asparagine", vec!["AAU", "AAC"]),
        ("histidine", vec!["CAU", "CAC"]),
        ("glutamic acid", vec!["GAA", "GAG"]),
        ("aspartic acid", vec!["GAU", "GAC"]),
        ("lysine", vec!["AAA", "AAG"]),
        ("arginine", vec!["CGU", "CGC", "CGA", "CGG", "AGA", "AGG"]),
        ("stop codon", vec!["UAA", "UAG", "UGA"]),
    ];
    let mut pairs = Vec::<(&'static str, &'static str)>::new();
    for (name, codons) in grouped.into_iter() {
        for codon in codons {
            pairs.push((codon, name));
        }
    }
    pairs.sort_by(|&(_, a), &(_, b)| a.cmp(b));
    pairs
}

fn main() {}
