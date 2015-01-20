package models_test

import (
	"testing"

	"Dust/models/test"
)

func TestModelsDuration(t *testing.T) {

	var testInstance = test.New()
	var testResult = testInstance.Duration()
	if testResult < 0 {
		t.Errorf("Negative duration %d", testResult)
	}
}
