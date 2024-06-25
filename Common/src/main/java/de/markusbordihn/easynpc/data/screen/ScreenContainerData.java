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

package de.markusbordihn.easynpc.data.screen;

import de.markusbordihn.easynpc.utils.UUIDUtils;
import java.util.UUID;
import net.minecraft.world.inventory.ContainerData;

public class ScreenContainerData implements ContainerData {

  // Order of the data fields, do not change the order!
  protected static final int SYNCED_INDEX = 0;
  protected static final int NPC_UUID_INDEX_START = 1;
  protected static final int NPC_UUID_INDEX_END = NPC_UUID_INDEX_START + 8;
  protected static final int NPC_UUID_INDEX_SIZE = NPC_UUID_INDEX_END - NPC_UUID_INDEX_START;
  protected static final int DIALOG_UUID_INDEX_START = NPC_UUID_INDEX_END + 1;
  protected static final int DIALOG_UUID_INDEX_END = DIALOG_UUID_INDEX_START + 8;
  protected static final int DIALOG_UUID_INDEX_SIZE =
      DIALOG_UUID_INDEX_END - DIALOG_UUID_INDEX_START;
  protected static final int DIALOG_BUTTON_UUID_INDEX_START = DIALOG_UUID_INDEX_END + 1;
  protected static final int DIALOG_BUTTON_UUID_INDEX_END = DIALOG_BUTTON_UUID_INDEX_START + 8;
  protected static final int DIALOG_BUTTON_UUID_INDEX_SIZE =
      DIALOG_BUTTON_UUID_INDEX_END - DIALOG_BUTTON_UUID_INDEX_START;
  protected static final int PAGE_INDEX = DIALOG_BUTTON_UUID_INDEX_END + 1;
  private static final int SIZE = PAGE_INDEX + 1;

  private final short[] data;

  public ScreenContainerData() {
    this(null);
    this.setSynced(false);
  }

  public ScreenContainerData(UUID npcUUID) {
    this(npcUUID, null, null, 0);
  }

  public ScreenContainerData(
      UUID npcUUID, UUID dialogUUID, UUID dialogButtonUUID, Integer pageIndex) {
    this.data = new short[SIZE];
    if (npcUUID != null) {
      this.setNpcUUID(npcUUID);
    }
    if (dialogUUID != null) {
      this.setDialogUUID(dialogUUID);
    }
    if (dialogButtonUUID != null) {
      this.setDialogButtonUUID(dialogButtonUUID);
    }
    this.setPageIndex(pageIndex);
    this.setSynced(true);
  }

  public boolean isSynced() {
    return get(SYNCED_INDEX) == 1;
  }

  public void setSynced(boolean synced) {
    set(SYNCED_INDEX, synced ? 1 : 0);
  }

  public UUID getNpcUUID() {
    short[] uuidShorts = new short[8];
    System.arraycopy(this.data, NPC_UUID_INDEX_START, uuidShorts, 0, NPC_UUID_INDEX_SIZE);
    return UUIDUtils.decodeShortToUUID(uuidShorts);
  }

  public void setNpcUUID(UUID uuid) {
    short[] uuidShorts = UUIDUtils.encodeUUIDToShort(uuid);
    System.arraycopy(uuidShorts, 0, this.data, NPC_UUID_INDEX_START, NPC_UUID_INDEX_SIZE);
  }

  public UUID getDialogUUID() {
    short[] uuidShorts = new short[8];
    System.arraycopy(this.data, DIALOG_UUID_INDEX_START, uuidShorts, 0, DIALOG_UUID_INDEX_SIZE);
    return UUIDUtils.decodeShortToUUID(uuidShorts);
  }

  public void setDialogUUID(UUID uuid) {
    short[] uuidShorts = UUIDUtils.encodeUUIDToShort(uuid);
    System.arraycopy(uuidShorts, 0, this.data, DIALOG_UUID_INDEX_START, DIALOG_UUID_INDEX_SIZE);
  }

  public UUID getDialogButtonUUID() {
    short[] uuidShorts = new short[8];
    System.arraycopy(
        this.data, DIALOG_BUTTON_UUID_INDEX_START, uuidShorts, 0, DIALOG_BUTTON_UUID_INDEX_SIZE);
    return UUIDUtils.decodeShortToUUID(uuidShorts);
  }

  public void setDialogButtonUUID(UUID uuid) {
    short[] uuidShorts = UUIDUtils.encodeUUIDToShort(uuid);
    System.arraycopy(
        uuidShorts, 0, this.data, DIALOG_BUTTON_UUID_INDEX_START, DIALOG_BUTTON_UUID_INDEX_SIZE);
  }

  public Integer getPageIndex() {
    return get(PAGE_INDEX);
  }

  public void setPageIndex(Integer pageIndex) {
    set(PAGE_INDEX, pageIndex);
  }

  @Override
  public int get(int i) {
    return this.data[i];
  }

  @Override
  public void set(int index, int value) {
    this.data[index] = (short) value;
  }

  @Override
  public int getCount() {
    return this.data.length;
  }
}
