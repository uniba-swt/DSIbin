#include <iostream>
#include <stdio.h>
//#include "Node.h"

class NestedNode {
	public:
		NestedNode *pnext;
		int payload01;
		int payload02;
		
		// Constructor taking initial value:
		NestedNode( int value = 0);

		// Insert node in front:
		void insert( NestedNode* newNode );

		// Iterate list starting from this element
		void iterate(std::string label);
};

NestedNode::NestedNode(int value){
	this->pnext = NULL;
	payload01 = value;
	payload02 = 2*value;
}

void NestedNode::insert(NestedNode *newNode){
	newNode->pnext = this->pnext;
	this->pnext = newNode;
}

// Iterate list starting from this element
void NestedNode::iterate(std::string label) {
	NestedNode* iter = this;
	while(iter) {
		std::cout << "\tChild Iterating elements " << std::endl;
		iter = iter->pnext;
	}
}

class Node {
public:
    Node* pnext;
    NestedNode child;
    int data;

    // Constructor taking initial value:
    Node( int value = 0 );

    // Insert node in front:
    void insert( Node* newNode );
	virtual void fake(Node* newNode)
	{
		this->insert(newNode);
	};

    // Remove node in front:
    void remove_next();

    // Calculate number of nodes in the list:
    size_t size() const;

    // Calculate distance to other node:
    int distance( Node const* other ) const;

    // Iterate list starting from this element
    void iterate(std::string label);
};

// Constructor taking initial value:
Node::Node( int value){
    this->data = value;
    this->pnext = NULL;
}

// Insert node in front:
void Node::insert( Node* newNode ){
    newNode->pnext = this->pnext;
    this->pnext = newNode;
}

// Remove node in front:
void Node::remove_next(){
    return;
}

// Calculate number of nodes in the list:
size_t Node::size() const {
    return 0;
}

// Calculate distance to other node:
int Node::distance( Node const* other ) const {
    return 0;
}

// Iterate list starting from this element
void Node::iterate(std::string label) {
	Node* iter = this;
	while(iter) {
		std::cout << label << "Iterating element" << std::endl;
		std::cout << label << "\tIterating children: " << std::endl;
		iter->child.iterate(" child: ");
		iter = iter->pnext;
	}
}

int main() {
	Node *head = new Node();
	printf("Head : %p, &next : %p, &data : %p\n", head, &head->pnext, &head->data);
	head->insert(new Node());
	head->insert(new Node());
	head->insert(new Node());
	head->insert(new Node());
	head->iterate("");
	Node *iter = head;
	while(iter) {
		std::cout << "Iterating parent node " << std::endl;
		NestedNode *childIter = &(iter->child);
		for(int i = 0; i < 5; i++){
			std::cout << "\tinserting child" << std::endl;
			childIter->insert(new NestedNode());
		}
		iter = iter->pnext;
	}
	head->iterate("");
}
