//
//  ProofsAxiomSchemas.swift
//  MathsNotebook
//
//  Created by Stephen Howe on 14/11/2023.
//

import Foundation

extension Proof {
    
    //func transform(resultId: ResultId, by rule: RewriteRule, with b: ExpressionBinding, in proofId: ProofId)
    func addAxiom(from schema: AxiomSchema, with b: ExpressionBinding, in proofId: ProofId) {
        // applies the expression binding to the schema and adds to proof
        // check proofId is valid
        guard valid(proofId: proofId) else { return }
        // get axiom
        if let axiom = schema.axiom(with: b) {
            // TODO: check for variable capture, i.e., if there are free variables in schema, can't use those in binding
            // add to results
            record(expressions: [axiom], as: ProofStep(type: .axiom, source: [], equivalence: true), at: proofId)
        }
    }
}

struct AxiomSchema {
    var name: String
    var description: String
    var namespace: String
    
    var schema: Expression
    
    var inputSpecifications: [InputSpecification]? = nil
    
    func axiom(with b: ExpressionBinding) -> Expression? {
        // check binding is consistent
        guard isConsistent(binding: b) else { return nil }
        // apply the binding to the schema
        return apply(binding: b, to: schema)
    }
}
