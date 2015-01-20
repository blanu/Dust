package models

import (
	"Dust/models/test"
)

func GetModels() map[string]Dust {
	var models = make(map[string]Dust)

	models["test"] = test.New()
	return models
}
