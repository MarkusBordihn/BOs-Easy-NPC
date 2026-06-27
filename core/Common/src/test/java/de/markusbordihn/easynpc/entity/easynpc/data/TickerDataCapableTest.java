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

package de.markusbordihn.easynpc.entity.easynpc.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import java.util.EnumMap;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import org.junit.jupiter.api.Test;

class TickerDataCapableTest {

  @Test
  void checkAndIncreaseTickerIncrementsLinearlyUntilThreshold() {
    TestTickerData tickerData = new TestTickerData();

    assertFalse(tickerData.checkAndIncreaseTicker(TickerType.TRADING_BASE_TICK, 3));
    assertEquals(1, tickerData.getTicker(TickerType.TRADING_BASE_TICK));

    assertFalse(tickerData.checkAndIncreaseTicker(TickerType.TRADING_BASE_TICK, 3));
    assertEquals(2, tickerData.getTicker(TickerType.TRADING_BASE_TICK));

    assertFalse(tickerData.checkAndIncreaseTicker(TickerType.TRADING_BASE_TICK, 3));
    assertEquals(3, tickerData.getTicker(TickerType.TRADING_BASE_TICK));

    assertTrue(tickerData.checkAndIncreaseTicker(TickerType.TRADING_BASE_TICK, 3));
    assertEquals(3, tickerData.getTicker(TickerType.TRADING_BASE_TICK));
  }

  @Test
  void resetTickerClearsStoredValue() {
    TestTickerData tickerData = new TestTickerData();
    tickerData.setTicker(TickerType.BASE_TICK, 7);

    tickerData.resetTicker(TickerType.BASE_TICK);

    assertEquals(0, tickerData.getTicker(TickerType.BASE_TICK));
  }

  private static final class TestTickerData implements EasyNPC<Mob>, TickerDataCapable<Mob> {

    private final EnumMap<TickerType, Integer> tickerMap = new EnumMap<>(TickerType.class);
    private int npcDataVersion;

    @Override
    public int getTicker(TickerType tickerType) {
      return this.tickerMap.getOrDefault(tickerType, 0);
    }

    @Override
    public void setTicker(TickerType tickerType, int value) {
      this.tickerMap.put(tickerType, value);
    }

    @Override
    public int getNPCDataVersion() {
      return this.npcDataVersion;
    }

    @Override
    public void setNPCDataVersion(int version) {
      this.npcDataVersion = version;
    }

    @Override
    public FakePlayer getFakePlayer(ServerLevel level, BlockPos blockPos) {
      return null;
    }

    @Override
    public <T> void defineSynchedEntityData(SynchedDataIndex synchedDataIndex, T defaultData) {}

    @Override
    public <T> void setSynchedEntityData(
        SynchedDataIndex synchedDataIndex, T data, boolean forceUpdate) {}

    @Override
    public <T> T getSynchedEntityData(SynchedDataIndex synchedDataIndex) {
      return null;
    }

    @Override
    public GoalSelector getEntityGoalSelector() {
      return null;
    }

    @Override
    public GoalSelector getEntityTargetSelector() {
      return null;
    }
  }
}
