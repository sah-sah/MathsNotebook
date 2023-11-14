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
    }
}

struct AxiomSchema {
    var name: String
    var description: String
    var namespace: String
    
    var schema: Expression
    
    var inputSpecifications: [InputSpecification]? = nil
    
    func axiom(with b: ExpressionBinding) -> Expression? {
        return nil
    }
}
