package lib

import "math"

// Helper function to calculate digits needed for a value (unsigned focus for PIC 9)
func CalculateWidthForValue(val int) int {
	// Handle potential negative result from subtractions for PIC 9 (unsigned)
	if val < 0 {
		val = -val // Get absolute value
	}

	if val == 0 {
		return 1
	}

	// Use Log10 for positive numbers
	return int(math.Log10(float64(val))) + 1
}
