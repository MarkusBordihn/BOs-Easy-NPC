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

package de.markusbordihn.easynpc.data.action;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PendingActionSet {

  public static final String DATA_PENDING_ACTION_SET_TAG = "Pending";
  public static final int MAX_PENDING_CHAINS = 16;
  public static final StreamCodec<RegistryFriendlyByteBuf, PendingActionSet> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public PendingActionSet decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new PendingActionSet(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, PendingActionSet pendingActionSet) {
          registryFriendlyByteBuf.writeNbt(
              EntityDataSerializersManager.validateAndGetNbt(
                  pendingActionSet.createTag(), "PendingActionSet"));
        }
      };

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private final Map<String, PendingActionChain> chains = new LinkedHashMap<>();

  public PendingActionSet() {}

  public PendingActionSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  private static String chainKey(ActionEventType actionEventType, Identifier sourceId) {
    String eventName =
        actionEventType != null ? actionEventType.name() : ActionEventType.NONE.name();
    return sourceId != null ? eventName + "@" + sourceId : eventName;
  }

  public void schedule(PendingActionChain pendingActionChain) {
    if (pendingActionChain == null) {
      return;
    }

    String chainKey = chainKey(pendingActionChain.actionEventType(), pendingActionChain.sourceId());
    if (this.chains.size() >= MAX_PENDING_CHAINS && !this.chains.containsKey(chainKey)) {
      log.warn(
          "Ignoring pending action chain {}, because {} chains are already pending!",
          chainKey,
          this.chains.size());
      return;
    }

    this.chains.put(chainKey, pendingActionChain);
  }

  public void cancel(ActionEventType actionEventType, Identifier sourceId) {
    this.chains.remove(chainKey(actionEventType, sourceId));
  }

  public void clear() {
    this.chains.clear();
  }

  public boolean has(ActionEventType actionEventType, Identifier sourceId) {
    return this.chains.containsKey(chainKey(actionEventType, sourceId));
  }

  public boolean isEmpty() {
    return this.chains.isEmpty();
  }

  public List<PendingActionChain> tickAndRemoveDueChains() {
    List<PendingActionChain> dueChains = new ArrayList<>();
    Iterator<Map.Entry<String, PendingActionChain>> chainIterator =
        this.chains.entrySet().iterator();
    while (chainIterator.hasNext()) {
      Map.Entry<String, PendingActionChain> chainEntry = chainIterator.next();
      PendingActionChain pendingActionChain = chainEntry.getValue().tick();
      if (pendingActionChain.isDue()) {
        dueChains.add(pendingActionChain);
        chainIterator.remove();
      } else {
        chainEntry.setValue(pendingActionChain);
      }
    }

    return dueChains;
  }

  public void load(CompoundTag compoundTag) {
    this.clear();
    if (compoundTag == null) {
      return;
    }

    ListTag listTag = compoundTag.getListOrEmpty(DATA_PENDING_ACTION_SET_TAG);
    for (int i = 0; i < listTag.size(); i++) {
      this.schedule(PendingActionChain.fromTag(listTag.getCompoundOrEmpty(i)));
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    if (this.chains.isEmpty()) {
      return compoundTag;
    }

    ListTag listTag = new ListTag();
    for (PendingActionChain pendingActionChain : this.chains.values()) {
      listTag.add(pendingActionChain.createTag());
    }
    compoundTag.put(DATA_PENDING_ACTION_SET_TAG, listTag);

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "PendingActionSet [" + this.chains + "]";
  }
}
