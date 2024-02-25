import React from "react";
import { Col, Container, Row } from "reactstrap";

export const Footer = () => {
  return (
    <footer className="footer">
      <Container fluid={true}>
        <Row>
          <Col md={12} className="footer-copyright text-center">
            <p className="mb-0">
              Copyright {new Date().getFullYear()} &copy; Cuba theme by
              pixelstrap
            </p>
          </Col>
        </Row>
      </Container>
    </footer>
  );
};
