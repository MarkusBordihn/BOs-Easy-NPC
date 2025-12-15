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

package de.markusbordihn.easynpc.data.scoreboard;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.Scoreboard;

public class ScoreboardData {

  private static final Pattern SCORE_PATTERN = Pattern.compile("@score\\(([a-zA-Z0-9_.-]+)\\)");
  private static final String SCORES_TAG = "Scores";

  private final Map<String, Integer> scores;

  public ScoreboardData() {
    this.scores = new HashMap<>();
  }

  public ScoreboardData(CompoundTag compoundTag) {
    this();
    if (compoundTag != null && compoundTag.contains(SCORES_TAG)) {
      CompoundTag scoresTag = compoundTag.getCompound(SCORES_TAG);
      for (String key : scoresTag.getAllKeys()) {
        this.scores.put(key, scoresTag.getInt(key));
      }
    }
  }

  public ScoreboardData(ServerPlayer serverPlayer, Set<String> objectiveNames) {
    this();
    if (serverPlayer != null && objectiveNames != null) {
      Scoreboard scoreboard = serverPlayer.getScoreboard();
      for (String objectiveName : objectiveNames) {
        if (isValidObjectiveName(objectiveName)) {
          this.scores.put(
              objectiveName, getScoreboardValue(scoreboard, serverPlayer, objectiveName));
        }
      }
    }
  }

  public static Set<String> parseScoreMacros(String text) {
    Set<String> objectiveNames = new HashSet<>();
    if (text == null || text.isEmpty()) {
      return objectiveNames;
    }

    Matcher matcher = SCORE_PATTERN.matcher(text);
    while (matcher.find()) {
      String objectiveName = matcher.group(1);
      if (isValidObjectiveName(objectiveName)) {
        objectiveNames.add(objectiveName);
      }
    }

    return objectiveNames;
  }

  private static boolean isValidObjectiveName(String objectiveName) {
    return objectiveName != null
        && !objectiveName.isEmpty()
        && objectiveName.length() <= 32
        && objectiveName.matches("[a-zA-Z0-9_.-]+");
  }

  private static int getScoreboardValue(
      Scoreboard scoreboard, ServerPlayer player, String objectiveName) {
    Objective objective = scoreboard.getObjective(objectiveName);
    if (objective != null) {
      return scoreboard.getOrCreatePlayerScore(player, objective).get();
    }
    return 0;
  }

  public int getScore(String objectiveName) {
    return this.scores.getOrDefault(objectiveName, 0);
  }

  public boolean hasScore(String objectiveName) {
    return this.scores.containsKey(objectiveName);
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    if (!this.scores.isEmpty()) {
      CompoundTag scoresTag = new CompoundTag();
      for (Map.Entry<String, Integer> entry : this.scores.entrySet()) {
        scoresTag.putInt(entry.getKey(), entry.getValue());
      }
      compoundTag.put(SCORES_TAG, scoresTag);
    }
    return compoundTag;
  }

  @Override
  public String toString() {
    return "ScoreboardData{scores=" + scores + "}";
  }
}
