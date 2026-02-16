/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.progression;

public class ProgressionLevelMap {

  public static final int MIN_LEVEL = 1;
  public static final int MAX_LEVEL = 60;

  private static final int[] XP_FOR_LEVEL = new int[MAX_LEVEL + 1];

  static {
    for (int level = 0; level <= MAX_LEVEL; level++) {
      XP_FOR_LEVEL[level] = calculateXpForLevel(level);
    }
  }

  private ProgressionLevelMap() {}

  public static int getExperienceForLevel(int level) {
    if (level <= MIN_LEVEL) return XP_FOR_LEVEL[MIN_LEVEL];
    if (level >= MAX_LEVEL) return XP_FOR_LEVEL[MAX_LEVEL];
    return XP_FOR_LEVEL[level];
  }

  public static int getExperienceForNextLevel(int level) {
    return level >= MAX_LEVEL ? XP_FOR_LEVEL[MAX_LEVEL] : XP_FOR_LEVEL[level + 1];
  }

  public static int getExperienceDifferenceForLevel(int level) {
    return level > MIN_LEVEL ? XP_FOR_LEVEL[level] - XP_FOR_LEVEL[level - 1] : 0;
  }

  private static int calculateXpForLevel(int level) {
    if (level <= 1) return 1;
    return (int) (Math.pow(level, 2.5) + (level * 10));
  }

  public static int getLevelForExperience(int experience) {
    if (experience <= XP_FOR_LEVEL[MIN_LEVEL]) return MIN_LEVEL;
    for (int level = MIN_LEVEL; level <= MAX_LEVEL; level++) {
      if (experience < XP_FOR_LEVEL[level]) return level - 1;
    }
    return MAX_LEVEL;
  }

  public static int getExperienceProgressToNextLevel(int experience, int level) {
    return level >= MAX_LEVEL ? 0 : experience - XP_FOR_LEVEL[level];
  }

  public static float getProgressPercentageToNextLevel(int experience, int level) {
    if (level >= MAX_LEVEL) return 100.0f;
    int currentLevelXP = XP_FOR_LEVEL[level];
    int nextLevelXP = XP_FOR_LEVEL[level + 1];
    int progress = experience - currentLevelXP;
    return (float) progress / (nextLevelXP - currentLevelXP) * 100.0f;
  }
}
