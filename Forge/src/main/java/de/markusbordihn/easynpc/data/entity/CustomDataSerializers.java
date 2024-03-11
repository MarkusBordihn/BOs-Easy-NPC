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

package de.markusbordihn.easynpc.data.entity;

import de.markusbordihn.easynpc.data.trading.TradingType;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.world.item.trading.MerchantOffers;

public class CustomDataSerializers {

  public static final EntityDataSerializer<TradingType> TRADING_TYPE =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, TradingType value) {
          buffer.writeEnum(value);
        }

        public TradingType read(FriendlyByteBuf buffer) {
          return buffer.readEnum(TradingType.class);
        }

        public TradingType copy(TradingType value) {
          return value;
        }
      };
  public static final EntityDataSerializer<MerchantOffers> MERCHANT_OFFERS =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, MerchantOffers value) {
          buffer.writeNbt(value.createTag());
        }

        public MerchantOffers read(FriendlyByteBuf buffer) {
          return new MerchantOffers(buffer.readNbt());
        }

        public MerchantOffers copy(MerchantOffers value) {
          return value;
        }
      };

  static {
    EntityDataSerializers.registerSerializer(MERCHANT_OFFERS);
    EntityDataSerializers.registerSerializer(TRADING_TYPE);
  }

  protected CustomDataSerializers() {}
}
