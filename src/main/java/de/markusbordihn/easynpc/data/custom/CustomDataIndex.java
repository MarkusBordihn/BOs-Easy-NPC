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

package de.markusbordihn.easynpc.data.custom;

import de.markusbordihn.easynpc.Constants;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public enum CustomDataIndex {

  // General Data Index (0-19) @formatter:off
  DATA_0,
  DATA_1,
  DATA_2,
  DATA_3,
  DATA_4,
  DATA_5,
  DATA_6,
  DATA_7,
  DATA_8,
  DATA_9,
  DATA_10,
  DATA_11,
  DATA_12,
  DATA_13,
  DATA_14,
  DATA_15,
  DATA_16,
  DATA_17,
  DATA_18,
  DATA_19,

  // Custom Data Index (>= 20)
  ACTION_EVENT_SET,
  DIALOG_DATA_SET,
  OBJECTIVE_DATA_SET,
  OBJECTIVE_PLAYER_SET,
  OBJECTIVE_ENTITY_SET,

  UNKNOWN; // @formatter:on

  public static final int MAX_FREE_INDEX = 20;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static CustomDataIndex getIndex(int index) {
    if (index < 0 || index > MAX_FREE_INDEX) {
      log.warn("Invalid data index {} is out of range (0-{})!", index, MAX_FREE_INDEX);
      return null;
    }
    return CustomDataIndex.values()[index];
  }
}
