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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.world.entity.PathfinderMob;

public interface TickerData<T extends PathfinderMob> extends EasyNPC<T> {

  int getTicker(TickerType tickerType);

  void setTicker(TickerType tickerType, int value);

  default void increaseTicker(TickerType tickerType) {
    increaseTicker(tickerType, 1);
  }

  default boolean checkAndIncreaseTicker(TickerType tickerType, int value) {
    int tickerValue = getTicker(tickerType);
    if (tickerValue >= value) {
      return true;
    }
    increaseTicker(tickerType, tickerValue + 1);
    return false;
  }

  default void resetTicker(TickerType tickerType) {
    setTicker(tickerType, 0);
  }

  default void increaseTicker(TickerType tickerType, int value) {
    setTicker(tickerType, getTicker(tickerType) + value);
  }
}
