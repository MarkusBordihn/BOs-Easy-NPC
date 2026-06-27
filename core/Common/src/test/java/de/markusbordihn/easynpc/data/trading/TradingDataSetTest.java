/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.data.trading;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class TradingDataSetTest {

  @Test
  void testDefaultValues() {
    TradingDataSet data = new TradingDataSet();
    assertEquals(TradingType.NONE, data.getType());
    assertEquals(64, data.getMaxUses());
    assertEquals(0, data.getRewardedXP());
    assertEquals(0, data.getResetsEveryMin());
    assertEquals(0L, data.getLastReset());
  }

  @Test
  @DisplayName("save() / load() round-trip preserves all fields")
  void testNbtRoundTrip() {
    TradingDataSet original = new TradingDataSet();
    original.setType(TradingType.BASIC);
    original.setMaxUses(1);
    original.setRewardedXP(5);
    original.setResetsEveryMin(30);
    original.setLastReset(123456789L);

    TradingDataSet loaded = new TradingDataSet(original.createTag());

    assertEquals(TradingType.BASIC, loaded.getType());
    assertEquals(1, loaded.getMaxUses());
    assertEquals(5, loaded.getRewardedXP());
    assertEquals(30, loaded.getResetsEveryMin());
    assertEquals(123456789L, loaded.getLastReset());
  }

  @Test
  @DisplayName("lastReset=0 survives round-trip")
  void testLastResetZeroRoundTrip() {
    TradingDataSet original = new TradingDataSet();
    original.setType(TradingType.BASIC);
    original.setLastReset(0L);

    TradingDataSet loaded = new TradingDataSet(original.createTag());

    assertEquals(0L, loaded.getLastReset());
  }

  @Test
  @DisplayName("legacy trading data without LastReset receives a non-zero fallback")
  void testLegacyDataWithoutLastResetUsesFallback() {
    CompoundTag inner = new CompoundTag();
    inner.putInt(TradingDataSet.DATA_TRADING_MAX_USES_TAG, 5);
    inner.putInt(TradingDataSet.DATA_TRADING_REWARDED_XP_TAG, 1);
    inner.putInt(TradingDataSet.DATA_TRADING_RESETS_EVERY_MIN_TAG, 10);
    inner.putString(TradingDataSet.DATA_TYPE_TAG, TradingType.BASIC.name());
    CompoundTag tag = new CompoundTag();
    tag.put(TradingDataSet.DATA_TRADING_DATA_SET_TAG, inner);

    TradingDataSet loaded = new TradingDataSet(tag);

    assertTrue(loaded.getLastReset() > 0L);
  }

  @Test
  @DisplayName("save() / load() round-trip preserves ADVANCED type")
  void testNbtRoundTripAdvanced() {
    TradingDataSet original = new TradingDataSet();
    original.setType(TradingType.ADVANCED);
    original.setMaxUses(9999);

    TradingDataSet loaded = new TradingDataSet(original.createTag());

    assertEquals(TradingType.ADVANCED, loaded.getType());
    assertEquals(9999, loaded.getMaxUses());
  }

  @Test
  void testLoadFromEmptyTagKeepsDefaults() {
    TradingDataSet data = new TradingDataSet(new CompoundTag());

    assertEquals(TradingType.NONE, data.getType());
    assertEquals(64, data.getMaxUses());
    assertEquals(0, data.getRewardedXP());
    assertEquals(0, data.getResetsEveryMin());
    assertEquals(0L, data.getLastReset());
  }

  @Test
  @DisplayName("maxUses=1 survives round-trip (one-off trade scenario)")
  void testSingleUseTradeRoundTrip() {
    TradingDataSet original = new TradingDataSet();
    original.setType(TradingType.BASIC);
    original.setMaxUses(1);

    TradingDataSet loaded = new TradingDataSet(original.createTag());

    assertEquals(1, loaded.getMaxUses());
    assertTrue(loaded.isType(TradingType.BASIC));
  }

  @Test
  @DisplayName("isType() returns false for null and mismatched types")
  void testIsType() {
    TradingDataSet data = new TradingDataSet();
    data.setType(TradingType.BASIC);

    assertTrue(data.isType(TradingType.BASIC));
    assertFalse(data.isType(TradingType.ADVANCED));
    assertFalse(data.isType(null));
  }

  @Test
  void testOfferActionDefaultsEmpty() {
    TradingDataSet data = new TradingDataSet();

    assertFalse(data.hasOfferAction(0));
    assertFalse(data.hasOfferAction(99));
    assertTrue(data.getOfferAction(0).isEmpty());
    assertTrue(data.getOfferActions().isEmpty());
  }

  @Test
  @DisplayName("setOfferAction / hasOfferAction / getOfferAction / remove work correctly")
  void testOfferActionAccessors() {
    TradingDataSet data = new TradingDataSet();
    ActionDataSet actions = new ActionDataSet();
    actions.add(new ActionDataEntry(ActionDataType.COMMAND, "/say hello"));

    data.setOfferAction(2, actions);

    assertTrue(data.hasOfferAction(2));
    assertFalse(data.hasOfferAction(0));
    assertEquals(1, data.getOfferAction(2).size());

    data.setOfferAction(2, null);
    assertFalse(data.hasOfferAction(2));

    data.setOfferAction(3, actions);
    data.setOfferAction(3, new ActionDataSet());
    assertFalse(data.hasOfferAction(3));
  }

  @Test
  @DisplayName("offer action survives NBT round-trip")
  void testOfferActionNbtRoundTrip() {
    TradingDataSet original = new TradingDataSet();
    original.setType(TradingType.ADVANCED);
    ActionDataSet actions = new ActionDataSet();
    actions.add(new ActionDataEntry(ActionDataType.COMMAND, "/give @s diamond 1"));
    original.setOfferAction(0, actions);

    TradingDataSet loaded = new TradingDataSet(original.createTag());

    assertTrue(loaded.hasOfferAction(0));
    assertFalse(loaded.hasOfferAction(1));
    assertEquals(1, loaded.getOfferAction(0).size());
    String loadedCommand = loaded.getOfferAction(0).getEntries().iterator().next().command();
    assertEquals("/give @s diamond 1", loadedCommand);
  }

  @Test
  @DisplayName("empty offerActions map writes no OfferActions tag to NBT")
  void testEmptyOfferActionsNotWrittenToNbt() {
    TradingDataSet data = new TradingDataSet();
    data.setType(TradingType.BASIC);

    CompoundTag tag = data.createTag();
    CompoundTag inner = tag.getCompound(TradingDataSet.DATA_TRADING_DATA_SET_TAG);

    assertFalse(inner.contains(TradingDataSet.DATA_OFFER_ACTIONS_TAG));
  }

  @Test
  @DisplayName("multiple offer actions at different indices all survive NBT round-trip")
  void testMultipleOfferActionsNbtRoundTrip() {
    TradingDataSet original = new TradingDataSet();
    original.setType(TradingType.ADVANCED);

    ActionDataSet actions0 = new ActionDataSet();
    actions0.add(new ActionDataEntry(ActionDataType.COMMAND, "/say offer0"));
    original.setOfferAction(0, actions0);

    ActionDataSet actions4 = new ActionDataSet();
    actions4.add(new ActionDataEntry(ActionDataType.COMMAND, "/say offer4"));
    original.setOfferAction(4, actions4);

    TradingDataSet loaded = new TradingDataSet(original.createTag());

    assertTrue(loaded.hasOfferAction(0));
    assertTrue(loaded.hasOfferAction(4));
    assertFalse(loaded.hasOfferAction(1));
    assertEquals("/say offer0", loaded.getOfferAction(0).getEntries().iterator().next().command());
    assertEquals("/say offer4", loaded.getOfferAction(4).getEntries().iterator().next().command());
  }
}
