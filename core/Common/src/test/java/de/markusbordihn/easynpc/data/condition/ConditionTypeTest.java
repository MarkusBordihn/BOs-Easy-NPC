/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.data.condition;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class ConditionTypeTest {

  @Test
  void testGet() {
    assertEquals(ConditionType.SCOREBOARD, ConditionType.get("SCOREBOARD"));
    assertEquals(ConditionType.EXECUTION_LIMIT, ConditionType.get("EXECUTION_LIMIT"));
    assertEquals(ConditionType.HAS_ITEM_IN_INVENTORY, ConditionType.get("HAS_ITEM_IN_INVENTORY"));
    assertEquals(ConditionType.HAS_ITEM_IN_MAIN_HAND, ConditionType.get("HAS_ITEM_IN_MAIN_HAND"));
    assertEquals(ConditionType.HAS_ITEM_IN_OFFHAND, ConditionType.get("HAS_ITEM_IN_OFFHAND"));
    assertEquals(ConditionType.ADVANCEMENT, ConditionType.get("ADVANCEMENT"));
    assertEquals(ConditionType.EXPERIENCE_LEVEL, ConditionType.get("EXPERIENCE_LEVEL"));
    assertEquals(ConditionType.PLAYER_HEALTH, ConditionType.get("PLAYER_HEALTH"));
    assertEquals(ConditionType.PLAYER_TAG, ConditionType.get("PLAYER_TAG"));
    assertEquals(ConditionType.TEAM, ConditionType.get("TEAM"));
    assertEquals(ConditionType.GAMEMODE, ConditionType.get("GAMEMODE"));
    assertEquals(ConditionType.FALLBACK, ConditionType.get("FALLBACK"));
    assertEquals(ConditionType.NONE, ConditionType.get("NONE"));
  }

  @Test
  void testGetInvalid() {
    assertEquals(ConditionType.NONE, ConditionType.get("INVALID"));
    assertEquals(ConditionType.NONE, ConditionType.get(""));
    assertEquals(ConditionType.NONE, ConditionType.get(null));
  }

  @Test
  void testRequiresName() {
    assertTrue(ConditionType.SCOREBOARD.requiresName());
    assertTrue(ConditionType.HAS_ITEM_IN_INVENTORY.requiresName());
    assertTrue(ConditionType.HAS_ITEM_IN_MAIN_HAND.requiresName());
    assertTrue(ConditionType.HAS_ITEM_IN_OFFHAND.requiresName());
    assertTrue(ConditionType.ADVANCEMENT.requiresName());
    assertTrue(ConditionType.PLAYER_TAG.requiresName());
    assertTrue(ConditionType.TEAM.requiresName());
    assertTrue(ConditionType.GAMEMODE.requiresName());
    assertFalse(ConditionType.EXECUTION_LIMIT.requiresName());
    assertFalse(ConditionType.EXPERIENCE_LEVEL.requiresName());
    assertFalse(ConditionType.PLAYER_HEALTH.requiresName());
    assertFalse(ConditionType.FALLBACK.requiresName());
    assertFalse(ConditionType.NONE.requiresName());
  }

  @Test
  void testRequiresValue() {
    assertTrue(ConditionType.SCOREBOARD.requiresValue());
    assertTrue(ConditionType.EXECUTION_LIMIT.requiresValue());
    assertTrue(ConditionType.EXPERIENCE_LEVEL.requiresValue());
    assertTrue(ConditionType.PLAYER_HEALTH.requiresValue());
    assertFalse(ConditionType.HAS_ITEM_IN_INVENTORY.requiresValue());
    assertFalse(ConditionType.HAS_ITEM_IN_MAIN_HAND.requiresValue());
    assertFalse(ConditionType.HAS_ITEM_IN_OFFHAND.requiresValue());
    assertFalse(ConditionType.ADVANCEMENT.requiresValue());
    assertFalse(ConditionType.PLAYER_TAG.requiresValue());
    assertFalse(ConditionType.TEAM.requiresValue());
    assertFalse(ConditionType.GAMEMODE.requiresValue());
    assertFalse(ConditionType.FALLBACK.requiresValue());
    assertFalse(ConditionType.NONE.requiresValue());
  }

  @Test
  void testRequiresOperation() {
    assertTrue(ConditionType.SCOREBOARD.requiresOperation());
    assertTrue(ConditionType.EXPERIENCE_LEVEL.requiresOperation());
    assertTrue(ConditionType.PLAYER_HEALTH.requiresOperation());
    assertFalse(ConditionType.EXECUTION_LIMIT.requiresOperation());
    assertFalse(ConditionType.HAS_ITEM_IN_INVENTORY.requiresOperation());
    assertFalse(ConditionType.HAS_ITEM_IN_MAIN_HAND.requiresOperation());
    assertFalse(ConditionType.HAS_ITEM_IN_OFFHAND.requiresOperation());
    assertFalse(ConditionType.ADVANCEMENT.requiresOperation());
    assertFalse(ConditionType.PLAYER_TAG.requiresOperation());
    assertFalse(ConditionType.TEAM.requiresOperation());
    assertFalse(ConditionType.GAMEMODE.requiresOperation());
    assertFalse(ConditionType.FALLBACK.requiresOperation());
    assertFalse(ConditionType.NONE.requiresOperation());
  }
}
